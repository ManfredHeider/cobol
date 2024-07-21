      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0551.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0551.CPv  $
      *
      *   Rev 2.1   Apr 09 2014 11:59:40   t13ex77
      *FKI01459: Fehlerverfahren - Vergrößern der Fehlerzähler
      *
      *   Rev 2.0   Feb 12 2013 13:25:34   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER: DRAT0551                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPAT0030                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * FKI01459!??.04.14!    ! Neuerstellung             ! abn       *
      *         !        !    ! Fehlerstatistik           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *---------------------------------------------------------------*
       01  DRAT0551.
           10  Fehlerstatistik-SATZ.
               20  FS-Fehler-SYSCODE           PIC X(02).
               20  FS-Fehler-Job-Name          PIC X(08).
               20  FS-Fehler-PGM               PIC X(08).
               20  FS-Fehler-GES               PIC 9(02).
               20  FS-Fehler-Nummer            PIC 9(04).
               20  FS-Fehler-Feldname          PIC X(15).
               20  FS-Fehler-Reaktion          PIC X(01).
               20  FS-Fehler-Relevanz          PIC X(01).
               20  FS-Fehler-Anzahl            PIC 9(09).
      *
      ******* ENDE COPY-MEMBER **************************************
