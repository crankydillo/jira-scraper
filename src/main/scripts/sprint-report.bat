@ECHO OFF
java -Dscallop.app.config="%~dp0/../conf/greenhopper-app.properties" -Dlog4j.configuration="%~dp0/../conf/log4j.properties" -cp "%~dp0/../lib/*;" org.beeherd.jira.SprintWorklogApp %*
