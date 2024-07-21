      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1015.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:56:44   T13AE18
      *apa00012
      *
      *   Rev 1.0   14 Jul 1998 13:59:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1015  FUNKTION                           *
      *                           GET ZAHLWEISE          (UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!14.07.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1015                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1015.
          05 PI-DRPA1015-DATEN.
             10 PI-DRPA1015-PRINTE           PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA1015-PVER-VERSION     PIC S9(004)  BINARY.
             10 PI-DRPA1015-S-PVER-VERSION   PIC X(001).
      *
       01 PO-DRPA1015.
          05 PO-DRPA1015-RC                  PIC  X(003).
             88 PO-DRPA1015-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1015-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1015-MELDUNG.
             10 PO-DRPA1015-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1015-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1015-DATEN.
             10 PO-DRPA1015-KNR-ZAHLWEISE         PIC S9(04) BINARY.
             10 PO-DRPA1015-KNR-ZAHLWEISE-BM      PIC X(010).
      *
      ******** ENDE COPY-MEMBER **************************************
