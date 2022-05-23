import {Command, Flags} from '@oclif/core'
import chokidar from 'chokidar'
import {promises as fs} from 'node:fs'
import WebSocket from 'ws'

export default class Watch extends Command {
  static description =
    'Watch the current folder for files & changes (must end in .sub,.sty,.dsl)';

  static examples = ['<%= config.bin %> <%= command.id %>'];

  static flags = {
    port: Flags.integer({
      char: 'p',
      description: 'websocket port to serve to frontend',
      default: 9160,
    }),
  };

  static args = [
    // { name: "file" }
  ];

  wss: WebSocket.Server | null = null;

  files: { substance: string[]; style: string[]; domain: string[] } = {
    substance: [],
    style: [],
    domain: [],
  };

  broadcastFiles() {
    if (this.wss) {
      for (const ws of this.wss.clients) {
        ws.send(JSON.stringify({kind: 'files', files: this.files}))
      }
    } else {
      console.warn('Websocket server not defined')
    }
  }

  async broadcastFileChange(fileName: string) {
    try {
      const contents = await fs.readFile(fileName, 'utf8')
      if (this.wss) {
        for (const ws of this.wss.clients) {
          ws.send(JSON.stringify({kind: 'file_change', fileName, contents}))
        }
      } else {
        console.warn('Websocket server not defined')
      }
    } catch (error) {
      console.warn(error)
    }
  }

  public async run(): Promise<void> {
    const {flags} = await this.parse(Watch)
    this.wss = new WebSocket.Server({port: flags.port})
    console.log(`watching on port ${flags.port}`)
    this.wss.on('connection', ws => {
      console.info('client connected')
      this.broadcastFiles()
      ws.on('message', async data => {
        const parsed = JSON.parse(data.toString())
        switch (parsed.kind) {
        case 'retrieve_file':
          this.broadcastFileChange(parsed.fileName)
          break
        default:
          console.error(`unknown message kind: ${parsed.kind}`)
        }
      })
      ws.on('close', () => {
        console.info('client disconnected')
      })
    })

    const watcher = chokidar.watch('.', {persistent: true})
    watcher.on('add', p => {
      switch (p.split('.').pop()) {
      case 'sub':
        this.files.substance.push(p)
        break
      case 'sty':
        this.files.style.push(p)
        break
      case 'dsl':
        this.files.domain.push(p)
        break
      }

      this.broadcastFiles()
    })
    watcher.on('error', err => {
      console.error(err)
    })
    watcher.on('change', async p => {
      if (['sub', 'sty', 'dsl'].includes(p.split('.').pop() ?? '')) {
        console.info(`file ${p} changed`)
        this.broadcastFileChange(p)
      }
    })
    watcher.on('unlink', p => {
      switch (p.split('.').pop()) {
      case 'sub':
        this.files.substance = this.files.substance.filter(f => f !== p)
        break
      case 'sty':
        this.files.style = this.files.style.filter(f => f !== p)
        break
      case 'dsl':
        this.files.domain = this.files.domain.filter(f => f !== p)
        break
      }

      this.broadcastFiles()
    })
  }
}
