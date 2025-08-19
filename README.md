# EU-JAMRAI 2: AMR surveillance dashboard


## Run the Shiny application locally

### For Linux:

#### Using RStudio
*todo*

#### Using Docker

- Install Docker (for Ubuntu, see: https://docs.docker.com/engine/install/ubuntu/)
- Clone the repository on your PC

    `git clone https://github.com/EU-JAMRAI-2-WP8-1/AMR-surveillance-dashboard.git`
- Enter the "AMR-surveillance-dashboard" directory
- Build and run the Docker image

    `docker compose build`

    `docker compose up`
- Open a browser at "http://localhost:8180/"
- To stop the application, press `ctrl-c` on the terminal

### For Windows

#### Using Rstudio
*todo*

#### Using Docker

- Be sure that Git is installed on your PC (https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
- Install Docker Desktop for Windows, see instructions at https://docs.docker.com/desktop/setup/install/windows-install/
- Clone the repository on your PC:
    - Open Git GUI
    - Click on "Clone Existing Repository"
    - In "Source location", write https://github.com/EU-JAMRAI-2-WP8-1/AMR-surveillance-dashboard.git
    - In "Target Directory", select a folder, then add the name for the new directory to be created at the end of the path (for example, select your "Document" folder, then add "/AMR-surveillance-dashboard
" to the path)
    - A GitHub window should pop-up, click on "Sign in with your browser". It opens a GitHub page on which you click on "Authorize git-ecosystem", then provide your credentials. The repository should now be cloned on your PC !
- Run "docker compose" in the folder (*more details will be added on this point*)

## Edit the application

- Navigate to the "amr-surveillance-dashboard" directory
- Create yourself a Git develoment branch (git branch dev-[username])
- Move to this new branch (git checkout dev-[username])
- Open the app.R file in a code editor
- Run the application locally to check you changes (see here above)
- When done, commit and push your changes on your distant development branch


### Remarks

- The app.R and style.css files are mapped to the Docker container, thus you do not need to rebuild the container if you edit these files (just restart it)
- You can use Rstudio to work locally on the graphs, then integrate your changes into app.R
- You can locally install R, Shiny and all the required packages (see Dockerfile), and run the app.R without using Docker. Be sure to adapt the resource file paths in app.R.
