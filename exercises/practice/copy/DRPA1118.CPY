      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1118.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1118.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:51:58   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:57:58   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1118                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1118                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    05.11.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!05.11.98! 01 ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1118
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1118 DES BUFFER-OZP ZYPA0027 ERMITTELT
      *    DIE ERGEBNISMENGE FüR PRODUKTUPRO TABELLE PRKO
      *
      ****************************************************************
      *
       01  PI-DRPA1118.
           05  PI-DRPA1118-UFKT                PIC  9(02).
           05  PI-DRPA1118-LOG-SATZ-NR         PIC  9(05).
           05  PI-DRPA1118-AUSWAHL-KRITERIEN.
               10 PI-DRPA1118-KTY-PGRP-ART     PIC  S9(04) COMP.
      *
       01  PO-DRPA1118.
           05  PO-DRPA1118-RC                  PIC  X(03).
               88 PO-DRPA1118-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1118-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1118-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1118-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA1118-ERGEBNISZEILE.
               10 PO-DRPA1118-PGEB-GES           PIC S9(004)     COMP.
               10 PO-DRPA1118-PROD-PROD-SCHL     PIC S9(004)     COMP.
               10 PO-DRPA1118-KNR-PGRP-ART-AUSPR PIC S9(004)     COMP.
               10 PO-DRPA1118-KTY-PGRP-ART-AUSPR PIC S9(004)     COMP.
               10 PO-DRPA1118-PGRP-NUMMER        PIC S9(009)     COMP.
               10 PO-DRPA1118-PPGR-GUELT-AB      PIC  X(010).
               10 PO-DRPA1118-PPGR-GUELT-BIS     PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
