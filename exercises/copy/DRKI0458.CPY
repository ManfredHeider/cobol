      * P13SP99   Stand: 06/12/98    18:01:41
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0458.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI0458.CPv  $
      *
      *   Rev 2.1   Jan 02 2008 17:30:40   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz PGM KUe
      *
      *   Rev 2.0   Apr 16 1999 13:20:10   T13EX19
      *aki00028
      *
      *   Rev 1.0   28 Jul 1998 09:55:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0458  FUNKTION                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    19.08.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!19.08.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      * --------!--------!----!---------------------------!----------*
      * AKL00666!19.12.07! 04 ! Neues Feld MANDANTNR      ! HSE      *
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0458                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DIREKTIONAENDERUNGSDATEN ERMITTELN <<<               *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0458.
          05 PI-DRKI0458-KEY.
             10 PI-DRKI0458-UFKT                PIC X(002).
      *
       01 PO-DRKI0458.
          05 PO-DRKI0458-RC                     PIC X(003).
             88 PO-DRKI0458-RC-OK                   VALUE 'OK '.
             88 PO-DRKI0458-RC-LEER                 VALUE 'LER'.
             88 PO-DRKI0458-RC-EOF                  VALUE 'EOF'.
      *
      ***    FEHLERPROTOKOLL
             88 PO-DRKI0458-RC-FEHLPROTOKOLL        VALUE 'FP '.
          05 PO-DRKI0458-DATEN.
             10 PO-DRKI0458-DIRAENDID           PIC X(013).
             10 PO-DRKI0458-DIRAENDPERSONENID   PIC X(013).
             10 PO-DRKI0458-DIRAENDPERSROLLE    PIC X(001).
             10 PO-DRKI0458-DIRAENDDTAERLEDIGT  PIC X(001).
             10 PO-DRKI0458-aenderungskz        pic 9(002).
             10 PO-DRKI0458-vertragsnr          pic 9(015).
             10 PO-DRKI0458-direktionsnr        pic 9(003).
             10 PO-DRKI0458-kundenbetreuernr    pic 9(007).
191207***    ZUSATZDATEN FUER Mandanten-Trennung wg. ALLFINANZ
191207       10 PO-DRKI0458-MANDANTNR           PIC 9(007).
      *
      ****** Ende Copy-Member ****************************************
