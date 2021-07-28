
The Bohemia Data Pipeline
=============================================

This repository contains utilities and guides created by DataBrew for use by the Bohemia research team. It is publicly available for the purposes of reproducibility and transparency.

Guides
------------

Several "how-to" guides are available for use by different Bohemia team members:  

### [1. Set up the server](guides/guide_odk_setup.md)

This is a step-by-step walkthrough showing how to set up the Bohemia data system from scratch. This includes everything from domain configuration and security certificates, to server-side software prerequisites and database configuration.  

### [2. Create forms](guides/guide_forms.md)

The Bohemia data system is built on the ODK framework, allowing for the creation and modification of survey forms specific to the Bohemia project's different study components. This guide shows how to create and deploy a form.

### [3. Enroll workers](guides/guide_enroll_workers.md)

This guide is intended for site-specific data managers. It assumes an up and running system with forms (guides 1 and 2).

### [4. Set up the tablet](guides/guide_data_collection_odk.md)

Once the system has been deployed and forms have been created, data collection software needs to be installed on android devices (phone or tablet). This guide shows how.

### [5. Fieldworker guide](guides/guide_fieldworker.md)

This guide is meant for fieldworkers, and provides an overview of how to collect and upload data.


### [6. How to retrieve data using ODK Briefcase](guides/guide_briefcase.md)

This guide shows how to export data from the Bohemia system to a local machine, for the purpose of exploration or analysis.

### [7. Automate backups](guides/guide_backups.md)



### [R package](guides/guide_r_package.md)

There is a stand-alone R package which has some tools and utilities for working with Bohemia data as well as the above pipeline. For documentation on the "bohemia" R package, including usage examples and how to build the package from scratch see the [the package page](rpackage/bohemia/README.md)

------------

Questions? info@databrew.cc
