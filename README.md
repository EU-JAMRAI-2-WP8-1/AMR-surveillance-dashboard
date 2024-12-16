# EU-JAMRAI 2: AMR surveillance dashboard


## Run the Shiny application locally

### For Linux:

- Install Docker (for Ubuntu, see: https://docs.docker.com/engine/install/ubuntu/)
- Clone the repository on your PC

    `git clone https://gitlab.com/eu-jamrai-2-wp8.1/amr-surveillance-dashboard.git`
- Enter the "amr-surveillance-dashboard" directory
- Build and run the Docker image

    `docker compose build`

    `docker compose up`
- Open a browser at "http://localhost:8180/"
- To stop the application, press ctrl-c on the terminal

### For Windows

*TODO*


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
