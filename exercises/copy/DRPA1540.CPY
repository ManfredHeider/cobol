      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1540.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1540.CPv  $
      *
      *   Rev 2.3   Nov 24 2015 12:23:40   t13ex44
      *APA02167: Lieferung Verträge ohne PIN an CKV
      *
      *   Rev 2.2   Nov 23 2015 16:11:14   t13ex44
      *APA02167: Lieferung Verträge ohne PIN an CKV
      *
      *   Rev 2.1   Sep 12 2002 14:47:22   t13ae46
      *APA00186: Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:34   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1540  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    10.06.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00186!10.06.02! 2.0! NEUERSTELLUNG             ! WANG     *
      * APA02167!09.11.15! 2.1! FELD 'SEL-ART' HINZU      ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1540                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1540.
          05 PI-DRPA1540-PROGRAMM           PIC  X(008).
          05 PI-DRPA1540-UFKT               PIC  9(002).
091115    05 PI-DRPA1540-SEL-ART            PIC  X(001).
          05 PI-DRPA1540-DATEN.
             10 PI-DRPA1540-PGEB-GES        PIC  S9(004) BINARY.
      *
       01 PO-DRPA1540.
          05 PO-DRPA1540-RC                 PIC  X(003).
             88 PO-DRPA1540-RC-OK           VALUE 'OK '.
             88 PO-DRPA1540-RC-LEER         VALUE 'LER'.
             88 PO-DRPA1540-RC-EOF          VALUE 'EOF'.
          05 PO-DRPA1540-DATEN.
             10 PO-DRPA1540-PINZ-PIN        PIC S9(009) COMP.
             10 PO-DRPA1540-PRINTE          PIC S9(015)V USAGE COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************
 

