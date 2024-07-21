      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1268.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1268.CPv  $
      *
      *   Rev 2.2   18 Apr 2001 08:59:44   t13ex21
      *FPA00220 - Neuen RC eingefügt
      *
      *   Rev 2.1   Jun 22 1999 23:21:12   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:18   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1268  FUNKTION AVERM/VM/KUNDENBETREUER   *
      *                           ERMITTELN                          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    21.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!21.06.99!2.1 ! NEUERSTELLUNG             !BOHLINGER *
      * FPA00220!11.04.01!2.2 ! NEUER RC-AUSPRÄGUNG       !STORCH    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1268                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1268.
          05 FILLER                        PIC  X(001).

       01 PO-DRPA1268.
          05 PO-DRPA1268-RC                PIC  X(003).
             88 PO-DRPA1268-RC-OK                      VALUE 'OK '.
             88 PO-DRPA1268-RC-MEHRERE-VM-IN-S         VALUE 'MVM'.
110401       88 PO-DRPA1268-RC-EMPF-KOMB-UNGUE         VALUE 'EKU'.

          05 PO-DRPA1268-DATEN.
             10 PO-DRPA1268-AVERM          PIC S9(009) BINARY.
             10 PO-DRPA1268-VM             PIC S9(009) BINARY.
             10 PO-DRPA1268-KD-BETR        PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************
