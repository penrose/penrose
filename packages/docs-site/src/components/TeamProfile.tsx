/**
 * Adopted from https://github.com/facebook/docusaurus/blob/main/website/community/1-team.mdx
 */

type ProfileProps = {
  className?: string;
  name: string;
  role: string;
  websiteUrl: string;
  githubUrl: string;
  twitterUrl?: string;
};

function TeamProfileCard({
  className,
  name,
  role,
  websiteUrl,
  githubUrl,
}: ProfileProps) {
  return (
    <div className={className}>
      <div className="avatar avatar--vertical">
        <a
          className="avatar__photo-link avatar__photo avatar__photo--xl"
          href={websiteUrl}
        >
          <img src={`${githubUrl}.png`} />
        </a>
        <div className="avatar__intro">
          <div className="avatar__name">{name}</div>
          <small className="avatar__subtitle">{role}</small>
        </div>
      </div>
    </div>
  );
}

function TeamProfileCardCol(props: ProfileProps) {
  return (
    <TeamProfileCard {...props} className="col col--3 margin-bottom--lg" />
  );
}

export function ActiveTeamRow(): JSX.Element {
  return (
    <div className="row">
      <TeamProfileCardCol
        name='Wode "Nimo" Ni'
        role="Ph.D. student @ CMU"
        websiteUrl="https://www.cs.cmu.edu/~woden/"
        githubUrl="https://github.com/wodeni"
      />
      <TeamProfileCardCol
        name="Max Krieger"
        role="🦆"
        websiteUrl="http://a9.io/"
        githubUrl="https://github.com/maxkrieger"
      />
      <TeamProfileCardCol
        name="Matt Davis"
        role="Ph.D. student @ CMU"
        websiteUrl="https://cmumatt.github.io/"
        githubUrl="https://github.com/cmumatt"
      />
      <TeamProfileCardCol
        name="Sam Estep"
        role="Ph.D. student @ CMU"
        websiteUrl="https://samestep.com/"
        githubUrl="https://github.com/samestep"
      />
      <TeamProfileCardCol
        name="Jiri Minarcik"
        role="Ph.D. student @ CTU"
        websiteUrl="https://github.com/jiriminarcik"
        githubUrl="https://github.com/jiriminarcik"
      />
      <TeamProfileCardCol
        name="Hwei-Shin Harriman"
        role="Software engineer @ Tableau"
        websiteUrl="https://hsharriman.github.io/"
        githubUrl="https://github.com/hsharriman"
      />
      <TeamProfileCardCol
        name="Yiliang (Leo) Liang"
        role="Undergraduate student @ UMich"
        websiteUrl=""
        githubUrl="https://github.com/liangyiliang"
      />
      <TeamProfileCardCol
        name="Josh Sunshine"
        role="Professor @ CMU"
        websiteUrl="https://www.cs.cmu.edu/~jssunshi/"
        githubUrl="https://github.com/joshsunshine"
      />
      <TeamProfileCardCol
        name="Jonathan Aldrich"
        role="Professor @ CMU"
        websiteUrl="https://www.cs.cmu.edu/~./aldrich/"
        githubUrl="https://github.com/JonathanAldrich"
      />
      <TeamProfileCardCol
        name="Keenan Crane"
        role="Professor @ CMU"
        websiteUrl="https://www.cs.cmu.edu/~kmcrane/"
        githubUrl="https://github.com/keenancrane"
      />
    </div>
  );
}
