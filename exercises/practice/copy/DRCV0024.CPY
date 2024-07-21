      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRCV0024.CPv  $
      *
      *   Rev 2.1   Dec 03 2013 11:37:24   t13ae35
      *ASL00498: Versorgungswerk 2013 - CTV-Anschreiben FAP-Weiterf
      *ührungsangebot COBOL
      *
      *   Rev 2.0   Dec 03 2013 10:55:36   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00498!03.12.13!2.1 ! Neuerstellung             ! J.Hering  *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER für CTV-BRIEFE BESONDERE ZUSATZLEISTG. *
      *            DATEINUMMER: CV011500 VSAM                         *
      *            COPYMEMBER:  DRCV0024                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0024-SATZ.
           02 DRCV0024-VB-NR                    PIC 9(007).
           02 DRCV0024-FAP-STF-ALT              PIC X(001).
           02 DRCV0024-FAP-STF-NEU              PIC X(001).
           02 DRCV0024-FAP-STF-MAN              PIC X(001).
           02 DRCV0024-FAP-AEND-KZ              PIC X(001).
           02 DRCV0024-FAP-SELBSTBETRAG         PIC Z(4)9,99.
      *
      **** ENDE DRCV0024

