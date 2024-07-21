      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1407.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1407.CPv  $
      *
      *   Rev 2.2   Feb 03 2003 10:09:02   t13ex21
      *FPA00524 - MA-Abrechnungsdatum hinzu
      *
      *   Rev 2.1   07 Sep 2000 14:56:42   T13EX23
      *APA00096 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:34   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1407                                      *
      *    --------------------------------------------------------   *
      *    UEBERGABEBEREICH FUER PROGRAMM UPPA0093                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                             *
      *    DATUM:    21.07.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00096!21.07.00! 2.1 ! NEUERSTELLUNG            ! GRAY     *
      * FPA00524!31.01.03! 2.2 ! DATUM-LETZTE-MA HINZU    ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT NUR DIE AUSGABEPARAMETER              *
      *    FÜR DIE DATENUEBERGABE VON UPPA0093 NACH DIALOG           *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      * GESAMT LAENGE DER FELDERN IN DIESER KOPIESTRECKE IST 2279    *
      ****************************************************************
      *
       01  PI-DRPA1407.
           05  PI-DRPA1407-LEER                 PIC  X(001).

       01  PO-DRPA1407.
           05  PO-DRPA1407-RC                   PIC  X(003).
               88 PO-DRPA1407-RC-OK                  VALUE 'OK '.
           05  PO-DRPA1407-DATEN.
               10 PO-DRPA1407-ALLG-STAT-PEG     PIC  X.
               10 PO-DRPA1407-ALLG-STAT-PVW     PIC  X.
               10 PO-DRPA1407-ALLG-NOTIZ-KZ     PIC  X.
               10 PO-DRPA1407-DAT-NAECHST-VB    PIC  X(10).
               10 PO-DRPA1407-DAT-NAECHST-MA    PIC  X(10).
310103         10 PO-DRPA1407-DAT-LETZTE-MA     PIC  X(10).
      *
      ******* ENDE COPY-MEMBER **************************************
