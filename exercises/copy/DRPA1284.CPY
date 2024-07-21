      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1284.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1284.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:29:16   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:54:02   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:19:52   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jul 27 1999 10:21:46   T13AE18
      *APA00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:24   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1284 FUNKTION LIST UMBUBUCHUGEN          *
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
      * APA01101!27.03.12!2.3 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1284                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1284.
          05 PI-DRPA1284-UFKT                    PIC  9(02).
          05 PI-DRPA1284-AUSWAHL-KRITERIEN.
             10 PI-DRPA1284-UMBUCH-DAT-VON       PIC X(010).
             10 PI-DRPA1284-UMBUCH-DAT-BIS       PIC X(010).
             10 PI-DRPA1284-PGEB-GES             PIC 9(002).
             10 PI-DRPA1284-S-PGEB-GES           PIC X(001).
             10 PI-DRPA1284-VNR                  PIC X(015).
             10 PI-DRPA1284-S-VNR                PIC X(001).
             10 PI-DRPA1284-BUCH-GRUND-GRP       PIC 9(002).
             10 PI-DRPA1284-S-BUCH-GRUND-GRP     PIC X(001).
             10 PI-DRPA1284-UMBUCH-KENNUNG       PIC X(008).
             10 PI-DRPA1284-S-UMBUCH-KENNUNG     PIC X(001).
             10 PI-DRPA1284-MANDANT              PIC 9(002).
             10 PI-DRPA1284-S-MANDANT            PIC X(001).
      *
       01 PO-DRPA1284.
          05  PO-DRPA1284-RC                     PIC  X(003).
              88 PO-DRPA1284-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1284-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA1284-RC-LEER                      VALUE 'LER'.
      *
          05  PO-DRPA1284-ERGEBNISZEILE.
              COPY DRPA1289.
      *
      ******** ENDE COPY-MEMBER **************************************
