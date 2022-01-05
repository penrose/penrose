import type { NextPage } from "next";
import Head from "next/head";
import Link from "next/link";
import styles from "../styles/Home.module.css";

const Home: NextPage = () => {
  return (
    <div className={styles.container}>
      <Head>
        <title>Penrose Docs</title>
        <meta name="description" content="documentation for Penrose oh yeah!" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main className={styles.main}>
        <h1>Penrose Docs</h1>
        <Link href={"/shapes"}>shapes</Link>
      </main>

      <footer className={styles.footer}>
        Made with {"<3"} in Pittsburgh and abroad
      </footer>
    </div>
  );
};

export default Home;
