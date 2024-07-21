      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.11.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0399.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0399.CPv  $
      *
      *   Rev 2.3   Apr 02 2013 17:29:04   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN - Anpassen M
      *odul ZXAT0035 + Aufrufende Programme
      *
      *   Rev 2.2   Feb 25 2004 15:18:16   t13ex39
      *AKI00379.UE001 BLZ
      *
      *   Rev 2.1   Feb 25 2004 14:40:32   t13ex39
      *AKI00379.UE001 BLZ
      *
      *   Rev 2.0   Nov 25 2003 11:16:42   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0399                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WHO                                              *
      *    DATUM:    11.11.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00379!11.11.03! 01 ! NEUERSTELLUNG             ! WHO      *
      * AVB01244!02.04.13! 02 ! Anpassung neue BLZ-Tabelle! B.PRETSCH*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRAT0399.
          05  PI-DRAT0399-PROGRAMM                PIC  X(008).
          05  PI-DRAT0399-UFKT                    PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05  PI-DRAT0399-DATEN.
              07 PI-DRKI11616CON-OBJECT-ID        PIC  X(013).

       01 PO-DRAT0399.

          05 PO-DRAT0399-RC                       PIC  X(003).
             88 PO-DRAT0399-RC-OK                    VALUE 'OK '.
             88 PO-DRAT0399-RC-LEER                  VALUE 'LER'.
             88 PO-DRAT0399-RC-EOF                   VALUE 'EOF'.

          05 PO-DRAT0399-DATEN.
             10 PO-DRAT0399-KNR-LAND-KZ           PIC S9(004) BINARY.
             10 PO-DRAT0399-BLZ                   PIC  X(011).
             10 PO-DRAT0399-KNR-NIEDERL-INTERN    PIC S9(004) BINARY.
             10 PO-DRAT0399-PLZ                   PIC  X(009).
             10 PO-DRAT0399-BIC                   PIC  X(011).
             10 PO-DRAT0399-NIEDERL-KZ            PIC  X(001).
             10 PO-DRAT0399-BANKNAME              PIC  X(058).
             10 PO-DRAT0399-KTY-LAND-KZ           PIC S9(004) BINARY.
             10 PO-DRAT0399-KTY-NIEDERL-INTERN    PIC S9(004) BINARY.
             10 PO-DRAT0399-BLZ-LAST-UPDATE       PIC  X(026).

