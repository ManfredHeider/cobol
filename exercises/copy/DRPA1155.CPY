      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1155.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1155.CPv  $
      *
      *   Rev 2.5   Jun 15 2016 07:42:12   t13ex44
      *FPA01517: Korrektur SST-MA
      *
      *   Rev 2.4   Aug 12 2015 12:24:04   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: MSC
      *
      *   Rev 2.3   25 Apr 2000 10:18:14   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.2   May 31 1999 14:28:42   T01JM2E
      *SORT-RFOLGE eingefügt
      *
      *   Rev 2.1   May 27 1999 11:00:32   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   10 Mar 1998 15:36:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1155                                      *
      *    --------------------------------------------------------   *
      *    Arbeitssatz MA-Abrechnung je PE                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    20.01.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.01.99! 01 ! NEU                       !D.WILHELM *
      * APA00012!31.05.99! 02 ! Einfügen SORT-RFOLGE      !D.WILHELM *
      *         !        !    !                           !          *
      * APA00084!14.03.00! 2.3! PAMS-DATEN DURCH COPY     ! KRUFF    *
      *         !        !    ! DRPA1070 ERSETZT          !          *
      * APA01932!10.08.15! 2.4! NEUTRALISIERUNG VIA KONFIG! SCHEIDEL *
      * FPA01517!15.06.16! 2.5! TABELLEN ERWEITERT        ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1155.
             05 PI-DRPA1155-MAX-INDEX     PIC S9(004)   BINARY SYNC.
             05 PI-DRPA1155-TAB-ZEILE  OCCURS 10000.
140300          10 PI-DRPA1155-PAMS-DATEN-EINE-PE.
140300             COPY DRPA1070.
                10 SORT-RFOLGE            PIC S9(004)    BINARY.
100815       05 PI-DRPA1155-S-NEUTRAL     PIC S9(004)    BINARY.
      *
       01 PO-DRPA1155.
             05 PO-DRPA1155-RC                  PIC  X(003).
                   88 PO-DRPA1155-RC-OK                     VALUE 'OK '.
                   88 PO-DRPA1155-RC-NOK                    VALUE 'NOK'.
             05 PO-DRPA1155-TAB-ZEILE  OCCURS 10000.
140300          10 PO-DRPA1155-PAMS-DATEN-EINE-PE.
140300             COPY DRPA1070.
                10 SORT-RFOLGE            PIC S9(004)    BINARY.
      *
      ******* ENDE COPY-MEMBER DRPA1155 *****************************
 

