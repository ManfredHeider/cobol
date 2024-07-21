      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1093.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1093.CPv  $
      *
      *   Rev 2.2   Oct 11 2016 16:01:48   t13ae37
      *APA02265: Einführung 4-stellige DGVs
      *
      *   Rev 2.1   Jul 29 2016 14:51:48   T13AE37
      *APA02265: Einführung 4-stellige DGVs
      *
      *   Rev 2.0   May 26 1999 16:46:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1093 FUNKTION wiederholte umbuchung      *
      *                          NACH EINER PE-STRUKTUR-äNDERUNG     *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    12.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.10.98! 01 !NEU                        !BOHLINGER *
      * APA02265!06.06.16!2.1 !DGV 4-STELLIG              !BUCH      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1093                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1093.
             10 PI-DRPA1093-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1093-MAX-USED-PDPT-TAB  PIC S9(004) BINARY.
             10 PI-DRPA1093-PDPT-ID-TAB        OCCURS 10.
                15 PI-DRPA1093-PDPT-ID         PIC S9(015)
                                               PACKED-DECIMAL.
060613*         15 PI-DRPA1093-DGV-BN          PIC  X(003).
060613          15 PI-DRPA1093-DGV-BN          PIC  X(004).
                15 PI-DRPA1093-S-DGV-BN        PIC  X(001).
 
 
       01 PO-DRPA1093.
          05 PO-DRPA1093-RC                    PIC  X(003).
             88 PO-DRPA1093-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1093-RC-UMB-UNZULAESSIG          VALUE 'UUZ'.
 
          05 PO-DRPA1093-MELDUNG.
             10 PO-DRPA1093-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1093-MELDUNG-NR         PIC S9(004) BINARY.
 
          05 PO-DRPA1093-VSTATUS               PIC  X(004).
 
          05 PO-DRPA1093-MAP-STEUERUNG.
            07 PO-DRPA1093-MAP-ST-TAB         OCCURS 10.
             10 PO-DRPA1093-MAP-ST-DGV.
                20 FELD-INTENSIV              PIC X(001).
                20 FELD-OFFEN                 PIC X(001).
                20 FELD-CURSOR-KZ             PIC X(001).
 
 
      *
      ******** Ende Copy-Member **************************************
 

