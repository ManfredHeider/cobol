      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0282.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0282.CPv  $
      *
      *   Rev 2.0   May 25 1999 13:05:42   T13AE18
      *apa00012
      *
      *   Rev 1.0   13 Jul 1998 09:28:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0282 FUNKTION NOTIZEN ZU MENGE VON       *
      *                                   OBJEKTEN                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    14.01.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!14.01.99! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0282                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN NOTIZEINTRäGE ZU MENGE VON OBJEKT     *
      *    << JE OBJEKT WIRD NUR DER SCHALTER 'NTZ-VORHANDEN'GESETZT *
      ****************************************************************
       01 PI-DRAT0282.
          05 PI-DRAT0282-AUSWAHL-KRITERIEN.
             10 PI-DRAT0282-MAX-USED-NTZ-TAB       PIC S9(004) BINARY.
             10 PI-DRAT0282-NTZ-TAB                OCCURS 15.
                20 PI-DRAT0282-KNR-OBJEKT-HKF-TAB  PIC S9(004) BINARY.
                20 PI-DRAT0282-NTZ-PTR-ID          PIC S9(015)
                                                   PACKED-DECIMAL.
                20 PI-DRAT0282-NTZ-PTR-ST-NR       PIC S9(009) BINARY.
                20 PI-DRAT0282-NTZ-PTR-PIN         PIC S9(009) BINARY.
                20 PI-DRAT0282-NTZ-PTR-VNR         PIC  X(015).
      *
       01 PO-DRAT0282.
          05 PO-DRAT0282-RC                        PIC  X(003).
             88 PO-DRAT0282-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0282-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRAT0282-DATEN.
             10 PO-DRAT0282-MAX-USED-S-NTZ         PIC S9(004) BINARY.
             10 PO-DRAT0282-S-NTZ-TAB              OCCURS 15.
                20 PO-DRAT0282-S-NTZ-VORHANDEN     PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
