# Single Page App Template (SPAT)
## Background

Recently I've had to build a Single Page Application at my place of employee that that made use of AngularJS,
a REST service implemented in Spring, and a database connection.  I could see myself and other creating something
similar, so I created this template to help myself and anyone else out.

## Technology Overview
### Frontend Technologies
1. **AngularJS** for the client side logic and single page app routing.  In the near future I'd like to explore doing this with AngularJS 2.0
2. **RequireJS** for managing JavaScript dependencies and load order.  This is a small application, but I had to make this design work for
a many page application that would eventually will have multiple team working on different feature simultaneously.  I found RequireJS is great
for keeping my JavaScript code small, organized, and specific for one purpose.  The folder structure I use is meant to scale for a large single
page application.
3. **Bootstrap** for my frontend widgets.  I have used the **AngularJS UI Bootstrap** library to remove the need for jQuery and the standard
jQuery that **Bootstrap** requires.
4. **LESS** CSS compiler for easy management of my stylesheets.
5. **GULP** I have a gulp script that can be used to minimfy, obfuscate, and compress the JavaScript code using **RequireJS's** **r.js** tool.
The index.jsp page has some logic to show how you can load which form of hte JavaScript you want depending if you are in development mode or in
production mode.

### Middleware Technologies
1. **Spring MVC** if the main middleware used to handle requests.  There are some stubbed out interceptors that can be expanded upon for logging
and authentication.
2. **SLF4J** and **Log4j** for logging.  There is a log42j.xml config in place that can be added to for additional logging and customization.

### Database Technologies
1. **PostgreSQL** I have included Maven dependencies and a basic spat.sql file for loading some simple test data for a PostgreSQL database.
Spring's JDBCTemplates were used for handling database queries. Obviously there are many ways to handle database interaction, so this just
serves as one way to show an end to end example.