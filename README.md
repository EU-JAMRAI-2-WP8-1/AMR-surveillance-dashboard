# EU-JAMRAI 2: AMR surveillance dashboard
## Proposing Changes to the development branch

[Development - Dashboard]([https://example.com](https://eu-jamrai-2-wp8-1.shinyapps.io/amr-surveillance-dashboard-development/))

---

## Overview

This guide outlines the process for proposing changes to the repository. Follow these steps to ensure a smooth and organized workflow.

---

## Steps to Propose Changes

1. **Start from the `development` Branch**
  - Always begin by pulling the latest changes from the `development` branch:
2. **Create a Feature Branch**
  - Create a new branch from `development` for the specific change you want to propose.
  - Name your branch using the format: `<your-name>-<component>-<description>`.  
  Example: If you are modifying `insight-tab1`, create a branch like:
    ```bash
    git checkout -b julius-insighttab1-text-update
    ```
3. **Make Your Changes**
  - Implement your changes in the new branch.
  - Commit your changes with clear and descriptive messages. Follow the [Commit Message Guidelines](#commit-message-guidelines) below.
    ```bash
    git add .
    git commit -m "Updated text in insight-tab1"
    ```
  - You can make multiple commits as needed.
4. **Push Your Branch**
  - Push your branch to the remote repository:
5. **Create a Pull Request (PR)**
  - Go to the repository on GitHub.
  - Create a new Pull Request to merge your branch into `development`.
  - Provide a clear title and description for your PR, explaining the changes you made.
6. **Review and Merge**
  - Baptiste and Julius will review your proposed changes.
  - If approved, they will merge your branch into `development`.

---

## Commit Message Guidelines

To maintain a clean and understandable commit history, follow these best practices:

1. **Use the Imperative Mood**
  - Write commit messages as if you are giving a command:
    - ✅ **"Add new feature"** (not "Added new feature" or "Adds new feature").
    - ✅ **"Fix bug in login"** (not "Fixed bug in login").
2. **Keep It Short and Clear**
  - **Subject Line**: Limit to **50 characters or less**. Summarize the change succinctly.
    - Example: `"Update README with contribution guidelines"`
  - **Body (Optional)**: Use for detailed explanations. Wrap at **72 characters** for readability.
3. **Separate Subject from Body**
  - Use a blank line between the subject and body (if a body is needed).
4. **Be Specific**
  - Avoid vague messages like `"Fix stuff"` or `"Update code"`.
  - Instead, specify **what** and **why**:
    - ✅ `"Fix login timeout error by increasing session duration"`
    - ✅ `"Refactor data processing for better performance"`
5. **Reference Issues or Tickets**
  - If your commit relates to a GitHub issue, or similar, include it:
    - `"Fix login bug (closes #123)"`
6. **Avoid Unnecessary Details**
  - Focus on **what changed** and **why**, not **how** (the code itself shows the "how").
7. **Use Bullet Points for Multiple Changes**
  - If a commit includes multiple related changes, list them clearly:
8. **Avoid Special Characters**
  - Stick to plain text. Avoid emojis, symbols, or non-ASCII characters unless they are part of a convention your team uses.

---

## Notes

- Ensure your branch is up-to-date with `development` before creating a PR.
- Keep your branch focused on a single feature or fix.
- Use meaningful commit messages to describe your changes.

---

c

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
