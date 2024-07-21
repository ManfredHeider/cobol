      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1105.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa1105.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:22:02   T01JM2S
      *APA00042
      *
      *   Rev 1.0   21 Oct 1998 13:56:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1105                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ÜBER PROT-ID   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    06.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!06.10.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1105
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1105.
           05 PI-DRPA1105-DATEN.
             10 PI-DRPA1105-PROGRAMM                PIC  X(008).
             10 PI-DRPA1105-UFKT                    PIC S9(004) BINARY.
             10 PI-DRPA1105-PROT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01  PO-DRPA1105.
           05  PO-DRPA1105-RC                       PIC  X(003).
               88 PO-DRPA1105-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1105-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1105-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1105-DATEN.
               10 PO-DRPA1105-PROZ.
                  15 PO-DRPA1105-PROT-ID            PIC S9(015)
                                                        PACKED-DECIMAL.
                  15 PO-DRPA1105-PROZ-ZEILE         PIC S9(004) BINARY.
                  15 PO-DRPA1105-PROZ-TEXT          PIC  X(080).
                  15 PO-DRPA1105-PROZ-BETRAG-JN     PIC  X(001).
                  15 PO-DRPA1105-PROZ-BETRAG        PIC S9(009)V99
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
