      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1313.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1313.CPv  $
      *
      *   Rev 2.3   15 May 2001 13:02:56   t13ex21
      *APA00096 - Occurs-Struktur entfernt
      *
      *   Rev 2.2   Mar 16 2000 16:20:26   T13EX13
      *APA00067 Anpassung an Badenia +
      *Umstellung PGDF -> PGKF
      *
      *   Rev 2.1   Feb 22 2000 08:46:50   T13AE18
      *APA00080
      *
      *   Rev 2.0   Nov 29 1999 13:55:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1313                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1313                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    06.12.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!06.12.99! 01 ! NEU                       ! BOHLINGER*
      * APA00096!12.10.00!2.01! ERHÖHUNG DER OCCURS-STRUK.! STORCH   *
      *         !24.04.01!2.02! OCCURS-STRUKTUR ENTFERNT  ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1313
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1313 DES BUFFER-OZP ZYPA0027 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR PRODUKTUPRO TABELLE PRKO
      *
      ****************************************************************
      *
       01  PI-DRPA1313.
           05  PI-DRPA1313-UFKT                PIC  9(02).
           05  PI-DRPA1313-LOG-SATZ-NR         PIC  9(05).
      *
       01  PO-DRPA1313.
           05  PO-DRPA1313-RC                  PIC  X(03).
               88 PO-DRPA1313-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1313-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1313-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1313-ANZAHL-ERG-ZEILEN   PIC  9(05).
070501*   05 PO-DRPA1313-ERGEBNISTABELLE.
140300*      10 PO-DRPA1313-PGKF-TAB                     OCCURS 100.
121000*      10 PO-DRPA1313-PGKF-TAB                     OCCURS 300.
070501*      10 PO-DRPA1313-PGKF-TAB                     OCCURS 200.
070501     05  PO-DRPA1313-ERGEBNISZEILE.
                    COPY DRPA1311.
      *
      ******* ENDE COPY-MEMBER **************************************
