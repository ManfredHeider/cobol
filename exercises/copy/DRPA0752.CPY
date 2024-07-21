      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0752.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0752.CPv  $
      *
      *   Rev 2.3   Jun 10 2008 10:30:56   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.2   03 Nov 2006 09:56:34   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jan 04 2000 13:09:18   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0752                                      *
      *    --------------------------------------------------------   *
      *    DATENFELDER FÜR DIALOG SF41 'PFLEGE DIALOGE'               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    10.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!10.11.99! 01 ! NEU                       ! BOHLINGER*
      * APA00452!02.11.06!2.2 ! UMBENENNUNG WG. 3-STELL.  ! WANG     *
      *         !        !    ! PRODUKTE                  !          *
      * APA00712!12.03.08!2.3 ! NEUES ATTRIBUT MANDANT    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG SF41                      *
      *                                                              *
      ****************************************************************
      *
      ***   DATENFELDER FÜR LISTDIALOG SF41
021106*        35 PROD-PROD-SCHL       PIC S9(004) COMP.
021106         35 PROD-PROD-SCHL       PIC S9(004) BINARY.
               35 PROD-BEGINN          PIC  X(010).
               35 PROD-ENDE            PIC  X(010).
               35 PROD-BEZ             PIC  X(040).
               35 PROD-BEZ-KURZ        PIC  X(010).
               35 PROD-LAST-UPDATE     PIC  X(026).
               35 PROD-KENNUNG         PIC  X(008).
120308         35 KNR-MANDANT          PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
