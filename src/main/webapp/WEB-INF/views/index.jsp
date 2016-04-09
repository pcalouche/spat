<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<!DOCTYPE html>
<html lang="en" ng-controller="MainNavigationController as vm">
<head>
    <title>Single Page App Template</title>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/resources/app/lib/bootstrap/3.3.6/css/bootstrap.min.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/resources/app/lib/font-awesome/4.5.0/css/font-awesome.min.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/resources/app/stylesheets/styles.css">
    <base href="${pageContext.request.contextPath}/">
</head>

<body ng-cloak>
    <nav class="navbar navbar-default">
        <div class="container-fluid">
            <div class="navbar-header">
                <button type="button" class="navbar-toggle collapsed" ng-click="vm.navbarCollapsed = !vm.navbarCollapsed">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="navbar-brand">SPAT</a>
            </div>

            <div class="navbar-collapse" uib-collapse="vm.navbarCollapsed">
                <ul class="nav navbar-nav">
                    <li ui-sref-active="active">
                        <a ui-sref="team-manager">My Teams</a>
                    </li>
                    <li ui-sref-active="active">
                        <a ui-sref="user-manager">User Manager</a>
                    </li>
                </ul>
            </div>
        </div>
    </nav>

    <div class="ui-view-container">
        <div style="" ui-view></div>
    </div>

    <%--Load RequireJS first, so we can add some config data to the app.module--%>
    <script src="${pageContext.request.contextPath}/resources/app/lib/require/2.1.22/require.min.js"></script>
    <%--Have a require.config here to capture some variables that need to be set in the JSP here that the app module will need--%>
    <script>
        require.config({
            config: {
                "app.module": {
                    spatGlobals: {
                        restServiceUrl: "http://localhost:8080${pageContext.request.contextPath}/",
                        version: "${version}"
                    }
                }
            }
        });
    </script>
    <%--Example of how to load optimized JavaScript versus non-optimized based on a server variable that is set in HomeController.java--%>
    <%--A version query parameter is added to the end to ensure the latest version is loaded inside of a browser cached version--%>
    <c:choose>
        <c:when test="${environment=='prod'}">
            <script src="${pageContext.request.contextPath}/resources/app/main-optimized.min.js?${version}"></script>
        </c:when>
        <c:otherwise>
            <script src="${pageContext.request.contextPath}/resources/app/main.js?${version}"></script>
        </c:otherwise>
    </c:choose>

</body>
</html>
