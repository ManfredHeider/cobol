      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1103.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa1103.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:20:10   T01JM2S
      *APA00042
      *
      *   Rev 1.0   21 Oct 1998 13:56:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1103                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<BUFFER PK13/PGPA0041>>                   *
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
      *    FÜR DIE FUNKTION DRPA1103
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1103 DES BUFFER-OZP ZYPA0026 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PK12
      *
      ****************************************************************
      *
       01  PI-DRPA1103.
           05  PI-DRPA1103-UFKT                     PIC  9(002).
           05  PI-DRPA1103-NAME-ZW-SPEICHER         PIC  X(008).
           05  PI-DRPA1103-LOG-SATZ-NR              PIC  9(005).
           05  PI-DRPA1103-AUSWAHL-KRITERIEN.
               10 PI-DRPA1103-PROT-ID               PIC  9(015).
      *
       01  PO-DRPA1103.
           05  PO-DRPA1103-RC                       PIC  X(003).
               88 PO-DRPA1103-RC-OK                       VALUE 'OK '.
               88 PO-DRPA1103-RC-EOF                      VALUE 'EOF'.
               88 PO-DRPA1103-RC-LEER                     VALUE 'LER'.
           05  PO-DRPA1103-ANZAHL-ERG-ZEILEN        PIC  9(005).

           05  PO-DRPA1103-ERGEBNISZEILE.
               10 PO-DRPA1103-TEXT                  PIC  X(080).
               10 PO-DRPA1103-BETRAG-JN             PIC  X(001).
               10 PO-DRPA1103-BETRAG                PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************
