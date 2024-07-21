      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0992.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0992.CPv  $
      *
      *   Rev 2.1   May 28 1999 18:31:04   T13EX26
      *Prod-Übergabe 28.0598 hpa
      *
      *   Rev 2.0   May 26 1999 16:58:14   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:21:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0992  FUNKTION GEGEN DGV ERMITTELN       *
      *                           ( FKPAW058 )                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    06.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !06.07.98! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRPA0992.
          05 PI-DRPA0992-DATEN.
             10 PI-DRPA0992-DGV                     PIC S9(004) BINARY.
             10 PI-DRPA0992-PRODART                 PIC S9(004) BINARY.
      *
       01 PO-DRPA0992.
          05 PO-DRPA0992-RC                         PIC  X(003).
             88 PO-DRPA0992-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0992-DATEN.
             10 PO-DRPA0992-GEGEN-DGV               PIC S9(004) BINARY.
             10 PO-DRPA0992-GEGEN-PRODART           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
