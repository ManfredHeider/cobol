      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1135.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1135.CPv  $
      *
      *   Rev 2.3   Jun 19 2012 09:28:18   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:53:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 12:25:42   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   May 26 1999 13:52:12   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:59:24   P13SP19
      *Initial revision.
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1135 FUNKTION PRUEFEN-KEYFELDER          *
      *                          FüR VORVERTRAGSDATEN-MODUL UGPA0003 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    23.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.11.98! 01 ! NEU                       !BOHLINGER *
      * APA00622!14.06.07!2.01! NEU: PO-KNR-VGES          ! STORCH   *
      * APA01101!05.03.12!2.02! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1135                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1135.
           05  PI-DRPA1135-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=K-VARIANTE-1
      *   FÜR DIALOGMODULE      10=K-VARIANTE-DIALOGM
          05 PI-DRPA1135-AUSWAHL-KRITERIEN.
             10 PI-DRPA1135-PGEB-GES        PIC X(002).
             10 PI-DRPA1135-VNR             PIC X(015).
          05 PI-DRPA1135-VERTRAGS-DATEN.
             10 PI-DRPA1135-PRINTE          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA1135-PIN             PIC S9(009) BINARY.
             10 PI-DRPA1135-HISTORIE        PIC S9(004) BINARY.
             10 PI-DRPA1135-PVER-KUNDE-NAME PIC X(025).
             10 PI-DRPA1135-KNR-PVER-ART    PIC S9(004) BINARY.
             10 PI-DRPA1135-PVER-PG-VBNR    PIC S9(009) BINARY.
      *
       01 PO-DRPA1135.
          05 PO-DRPA1135-RC                   PIC X(003).
             88 PO-DRPA1135-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1135-RC-NOK                VALUE 'NOK'.

          05 PO-DRPA1135-MELDUNG.
             10 PO-DRPA1135-MELDUNG-TYP       PIC  X(001).
             10 PO-DRPA1135-MELDUNG-NR        PIC S9(004) BINARY.

          05 PO-DRPA1135-MAP-STEUERUNG.
             10 PO-DRPA1135-MAP-ST-GES.
                20 PO-DRPA1135-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA1135-FELD-OFFEN     PIC X(001).
                20 PO-DRPA1135-FELD-CURSOR-KZ PIC X(001).

             10 PO-DRPA1135-MAP-ST-VNR.
                20 PO-DRPA1135-FELD-INTENSIV  PIC X(001).
                20 PO-DRPA1135-FELD-OFFEN     PIC X(001).
                20 PO-DRPA1135-FELD-CURSOR-KZ PIC X(001).

          05 PO-DRPA1135-DATEN.
             10 PO-DRPA1135-PRINTE           PIC S9(015) PACKED-DECIMAL.
             10 PO-DRPA1135-PGEB-GES         PIC S9(004) BINARY.
             10 PO-DRPA1135-PGEB-GES-BM      PIC  X(010).
             10 PO-DRPA1135-PVER-VNR         PIC  X(015).
             10 PO-DRPA1135-PVER-KUNDE-NAME  PIC  X(020).
             10 PO-DRPA1135-PVER-AKT-PIN     PIC S9(009) BINARY.
             10 PO-DRPA1135-KNR-PVER-ART     PIC S9(004) BINARY.
             10 PO-DRPA1135-PVER-PG-VBNR     PIC S9(009) BINARY.
             10 PO-DRPA1135-KNR-MANDANT      PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
