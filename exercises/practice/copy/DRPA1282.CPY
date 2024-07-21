      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1282.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1282.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:29:08   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:54   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:19:42   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jul 27 1999 10:21:40   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:22   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1282 FUNKTION LIST UMBUBUCHUGEN          *
      *                                   FÜR DIALOG PK14            *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    21.07.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!21.07.99! 01 !NEU                        !BOHLINGER *
      * APA00622!12.02.08!2.2 ! ERWEITERUNG UM VERTRIEBSG.! STORCH   *
      * APA01101!14.03.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1282                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1282.
          05 PI-DRPA1282-UFKT                    PIC  9(02).
          05 PI-DRPA1282-AUSWAHL-KRITERIEN.
             10 PI-DRPA1282-UMBUCH-DAT-VON       PIC X(010).
             10 PI-DRPA1282-UMBUCH-DAT-BIS       PIC X(010).
             10 PI-DRPA1282-PGEB-GES             PIC 9(002).
             10 PI-DRPA1282-S-PGEB-GES           PIC X(001).
             10 PI-DRPA1282-VNR                  PIC X(015).
             10 PI-DRPA1282-S-VNR                PIC X(001).
             10 PI-DRPA1282-BUCHUNGS-GRUND       PIC 9(002).
             10 PI-DRPA1282-S-BUCHUNGS-GRUND     PIC X(001).
             10 PI-DRPA1282-UMBUCH-KENNUNG       PIC X(008).
             10 PI-DRPA1282-S-UMBUCH-KENNUNG     PIC X(001).
             10 PI-DRPA1282-MANDANT              PIC 9(002).
             10 PI-DRPA1282-S-MANDANT            PIC X(001).
      *
       01 PO-DRPA1282.
          05  PO-DRPA1282-RC                     PIC  X(003).
              88 PO-DRPA1282-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1282-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA1282-RC-LEER                      VALUE 'LER'.
      *
          05 PO-DRPA1282-DATEN.
             10 PO-DRPA1282-MAX-USED-UMBUCH      PIC S9(004) BINARY.
             10 PO-DRPA1282-UMBUCH-TAB           OCCURS 20.
               15 PO-DRPA1282-ERGEBNISZEILE.
                  COPY DRPA1289.
      *
      ******** Ende Copy-Member **************************************
