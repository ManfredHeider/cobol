      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0272.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0272.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:31:58   T13AE35
      *AIS00009 - Neuerstellung, PI/PO fuer ZWVB0064
      *
      *   Rev 1.0   23 Jul 1997 12:24:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0272                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0272                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    17.06.1998                                       *
      *****************************************************************
      *    VERSION 1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00009!17.06.98!    ! NEU                       ! HERING   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0272:
      *    EINGABE:                         ST-NR
      *                                     SLM-DAT-VON
      *                                     KNR-SLM-TYP
      *    RUECKGABE:
      *                                     GANZER TUPEL VON SLM
      *                                     FACHLICHER RC
      ****************************************************************
      *
       01  PI-DRVB0272.
           05  PI-DRVB0272-DATEN.
               10 PI-DRVB0272-ST-NR            PIC S9(009) BINARY.
               10 PI-DRVB0272-SLM-DAT-VON      PIC  X(010).
               10 PI-DRVB0272-KNR-SLM-TYP      PIC S9(004) BINARY.
      *
       01  PO-DRVB0272.
           05  PO-DRVB0272-RC                  PIC  X(003).
               88 PO-DRVB0272-RC-OK            VALUE 'OK '.
               88 PO-DRVB0272-RC-NOT-FOUND     VALUE 'NFD'.
           05  PO-DRVB0272-DATEN.
               10 PO-DRVB0272-ST-NR             PIC S9(009) BINARY.
               10 PO-DRVB0272-SLM-DAT-VON       PIC X(10).
               10 PO-DRVB0272-KNR-SLM-TYP       PIC S9(004) BINARY.
               10 PO-DRVB0272-KNR-WAEHRUNG      PIC S9(004) BINARY.
               10 PO-DRVB0272-SLM-LAST-UPDATE   PIC X(26).
               10 PO-DRVB0272-KTY-SLM-TYP       PIC S9(004) BINARY.
               10 PO-DRVB0272-KTY-WAEHRUNG      PIC S9(004) BINARY.
               10 PO-DRVB0272-SLM-AUSZAHLUNG    PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
