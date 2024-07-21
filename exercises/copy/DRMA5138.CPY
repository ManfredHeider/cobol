      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5138.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5138.CPv  $
      *
      *   Rev 2.1   May 18 2016 11:25:16   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   Apr 25 2016 14:23:48   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: SST STATUSWECHSELTABELLE
      *
      *     SOURCE-NAME      :  DRMA5138
      *
      *     GENERIERUNG      :  13.05.2016/11:36:42
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00263!22.04.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5138.
              07 VBNR                   pic 9(7).
              07 BUCHDAT                pic X(10).
              07 BEZUGSJAHR             pic X(4).
              07 STEUERJAHR             pic X(4).
              07 HERKUNFT-KZN           pic X.
              07 ABR-NR                 pic X(15).
              07 KNR-VERARB-TYP         pic S9(4)      binary.
              07 SH-KZN                 pic X.
              07 BUCHTEXT               pic X(40).
              07 BUCHTEXT2              pic X(52).
              07 KNR-USTAT-TYP          pic S9(4)      binary.
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 KTO-KZN                pic X.
              07 BELEGART               pic XXX.
              07 BELEGNR                pic X(7).
              07 BUCHPERIODE            pic X(6).
              07 BELEGDATUM             pic X(8).
              07 KOSTENSTELLE           pic X(8).
              07 KONTO-KU               pic X(12).
              07 BETRAG-KU              pic S9(11)V99  packed-decimal.
              07 KONTO-U                pic X(12).
              07 BETRAG-U               pic S9(11)V99  packed-decimal.
              07 UST-U                  pic S9(11)V99  packed-decimal.
              07 UST-SCHLUESSEL         pic S9(4)      binary.
              07 UST-BERECH-KZN         pic X.
      *
      **************************************************************

