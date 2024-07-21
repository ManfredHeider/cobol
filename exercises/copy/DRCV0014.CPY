      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRCV0014.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRCV0014.CPv  $
      *
      *   Rev 2.4   Nov 26 2013 13:59:50   t13ae35
      *ASL00469: CTV-Anpassungen
      *
      *   Rev 2.3   Nov 22 2013 14:36:48   t13ae30
      *ASL00469: CTV-Anpassungen
      *
      *   Rev 2.2   Nov 27 2009 15:18:26   t13ex43
      *ASL00272: PGM - CTV-Briefe Besondere Zusatzleistungen
      *
      *   Rev 2.1   Oct 23 2008 10:56:58   t13ae35
      *ASL00310: Besondere Zusatzleistungen für 2009
      *
      *   Rev 2.0   Oct 23 2008 10:16:20   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00310!23.10.08!2.1 ! Neuerstellung             ! J.Hering  *
251109* ASL00272!25.11.09!2.2 ! ANPASSUNGEN FÜR 2010      ! KLEEMEIER *
      * ASL00469!22.11.13!2.3 ! ANPASSUNGEN 2013          ! WILLIG    *
261113* ASL00469!26.11.13!2.4 ! zusätzlich VERS-NR Central! J.Hering  *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *            COPY-MEMBER für CTV-BRIEFE BESONDERE ZUSATZLEISTG. *
      *            DATEINUMMER: CV004700 QSAM                         *
      *            COPYMEMBER:  DRCV0014                              *
      *            SATZLAENGE:  27 BYTES --> 2010: 19 BYTES           *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRCV0014-SATZ.
           02 DRCV0014-VB-NR                    PIC 9(007).
           02 DRCV0014-BZL-NEU-KZ               PIC X(001).
251109     02 DRCV0014-BZL-CENTRAL-KZ           PIC X(001).
251109     02 DRCV0014-BZL-AUFSTOCK-KZ          PIC X(001).
           02 DRCV0014-BZL-BETRAG               PIC Z(5)9,99.
221113     02 DRCV0014-GEB-DAT                  PIC X(010).
261113     02 DRCV0014-CEN-VNR                  PIC X(015).
      *
      **** ENDE DRCV0014
      *
