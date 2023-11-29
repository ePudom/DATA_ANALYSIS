USE master;
GO

IF NOT EXISTS (
      SELECT name
      FROM sys.databases
      WHERE name = N'DBNAME'
      )
   CREATE DATABASE [DBNAME];
GO

IF SERVERPROPERTY('ProductVersion') > '12'
   ALTER DATABASE [DBNAME] SET QUERY_STORE = ON;
GO