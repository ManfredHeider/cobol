      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1211.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1211.CPv  $
      *
      *   Rev 2.1   Apr 21 1999 09:25:10   T13AE18
      *APA00012 Neu
      *
      *   Rev 2.0   Mar 30 1999 08:44:02   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1211 FUNKTION SET FäLLIG-BIS FüR         *
      *                          OPAL-PROVISIONSMELDUNGEN            *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    13.04.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.04.99! 01  !NEU                       !BOHLINGER *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1211                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1211.
             10 PI-DRPA1211-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1211-PMBE-ID            PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1211-FAELLIG-BIS        PIC  X(010).

       01 PO-DRPA1211.
          05 PO-DRPA1211-RC                    PIC  X(003).
             88 PO-DRPA1211-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1211-RC-UNZULAESSIG              VALUE 'UNZ'.


          05 PO-DRPA1211-MELDUNG.
             10 PO-DRPA1211-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1211-MELDUNG-NR         PIC S9(004) BINARY.
      *

          05 PO-DRPA1211-MAP-STEUERUNG.
             10 PO-DRPA1211-MAP-ST-FAELLIG-B.
                20 FELD-INTENSIV              PIC X(001).
                20 FELD-OFFEN                 PIC X(001).
                20 FELD-CURSOR-KZ             PIC X(001).
      ******** Ende Copy-Member **************************************
