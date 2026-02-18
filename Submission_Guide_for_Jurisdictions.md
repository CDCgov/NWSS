# Information for Jurisdictions Submitting Code

## Overview

To promote transparency, reproducibility, and shared learning, this repository is intended for collaboration among jurisdictions, CDC, and the public. **All repository contents—including forks, branches, issues, and pull requests—are fully public.**  
**Do not submit any material that is not appropriate for public release.**

Jurisdictions are encouraged to contribute code, analytic workflows, and tools that support NWSS program goals. Please follow the guidance below when adding materials to this repository. Additionally, CDC NWSS does not review submitted code for errors and cannot guarantee that the code will function as intended.

## 1. Contribute Through Fork and Pull Request

All contributions must use a standard GitHub fork-and-pull-request workflow:

1. **Fork** the repository to your own GitHub account or organization.  
2. **Develop** your contributions within your fork.  
3. **Submit a pull request** back to the NWSS public repository.

If you are new to GitHub, consult GitHub’s introductory documentation on forking and pull requests.

## 2. Create a Jurisdiction Subdirectory

Each jurisdiction should create a dedicated subdirectory under jurisdiction-scripts/

Name your directory using the jurisdiction abbreviation used for `reporting_jurisdiction` in 1CDP (e.g., `CA`, `IL`, `NZ`).  
If your directory already exists, add new content within that folder.

## 3. Add Each Project or Analytic Workflow as Its Own Folder

Within your jurisdiction directory, create a separate folder for each contributed project, tool, or analytic workflow.

## 4. Do Not Include Sensitive Information

Because all contributions are publicly accessible, verify that **no sensitive or restricted information** is included.

This includes, but is not limited to:

- Personally identifiable information (PII)  
- Protected Health Information (PHI)  
- API keys, passwords, tokens, or other credentials  
- Restricted or proprietary jurisdictional datasets  
- Any information not intended for public dissemination  

## 5. Include a README for Each Project

Each project folder must include a `README.md` that contains:

- A brief overview of the project and its purpose  
- A statement that the project is created and maintained by your jurisdiction  
- Contact information (e.g., jurisdiction program email)  
- Installation instructions, dependencies, and how to run the code  
- Notes on data needs, assumptions, limitations, or intended use  

## 6. (Strongly Recommended) Exclude Large or Reproducible Dependencies

To keep the repository lightweight and reproducible:

- Do **not** include downloaded or cached libraries  
  (e.g., R package directories, Python virtual environments).  
- Instead, declare dependencies using standard environment or package-management files, such as:
  - `renv` for R  
  - `requirements.txt` for Python  

