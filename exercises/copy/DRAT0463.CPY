      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0463.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0463.CPv  $
      *
      *   Rev 2.1   Apr 13 2010 09:45:46   t13ex57
      *AVB01053: Kto.Nr.-Prüfung in OSTA 
      *
      *   Rev 2.0   Aug 03 2009 16:58:04   t13ex57
      *Archiverstellung
      *
      *    COPY-MEMBER: DRAT0463                                      *
      *---------------------------------------------------------------*
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *---------------------------------------------------------------*
      *    ERSTELLT: BOLL                                             *
      *    DATUM:    29.07.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01053!29.07.09! 02 !Ermittlung des Länderkz    ! U.BOLL   *
      *         !        !    !für Kto prüfung            !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0463                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0463.
          05 PI-DRAT0463-BLZ                PIC  X(09)  .
      *
       01 PO-DRAT0463.
          05 PO-DRAT0463-RC                 PIC  X(003) .
             88 PO-DRAT0463-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0463-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRAT0463-DATEN.
            10 PO-DRAT0463-KTOLAND          PIC S9(04) COMP.
      *
      ***** Ende Copy-Member DRAT0463 *****************************
