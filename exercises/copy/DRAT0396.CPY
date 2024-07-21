      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0396.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0396.CPv  $
      *
      *   Rev 2.3   Feb 27 2013 17:10:46   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.2   Nov 19 2012 16:43:30   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.1   Sep 05 2003 13:54:38   t13ae30
      *h:\mfe\cpy\DRAT0396.CPY -> L:\mf\CPY\DRAT0396.CPv
      *AVB00536 - PLAUSIBILISIERUNG DER BLZ
      *
      *   Rev 2.0   26 Jun 2001 12:55:18   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0396                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00536!04.08.03! 01 ! NEUERSTELLUNG             !TH.WILLIG *
      * AVB01244!15.11.12! 02 ! SEPA-Anp. mit BIC-Zugriff !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0396                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0396.
          05 PI-DRAT0396-BLZ                PIC  X(009) .
      *
       01 PO-DRAT0396.
          05 PO-DRAT0396-RC                 PIC  X(003) .
             88 PO-DRAT0396-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0396-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRAT0396-DATEN.
            10 PO-DRAT0396-BANKNAME         PIC  X(058) .
            10 PO-DRAT0396-BIC              PIC  X(011) .
      *
      ***** Ende Copy-Member DRAT0396 *****************************
