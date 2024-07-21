      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0069.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DWAT0069.CPv  $
      *
      *   Rev 2.1   Jan 24 2017 08:27:54   t13ac20
      *AAT00056: Auslagerung DB2-Befehle
      *
      *   Rev 2.0   Jan 23 2017 16:12:36   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DWAT0069                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: DATENFELDER ZU DB2-BEFEHLEN                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SWO                                              *
      *    DATUM:    23.01.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00056!23.01.17!2.1 ! NEU                       ! SWO       *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    KONSTANTEN FÜR UNTERPROGRAMMAUFRUFE
       01  K-UPAT0042            PIC  X(008) VALUE 'UPAT0042'.
      *
      *    PARAMETER FÜR UPAT0042
           COPY DRAT0613.
           COPY DRAT0614.
           COPY DRAT0615.
           COPY DRAT0616.
      *
      *    LAUFDATUM IM FORMAT JJJJ-MM-TT-HH.MM.SS.HSSSSS
      *                        (DB2-TIMESTAMP AUS DB2)
       01  BR-Z-LAUFDATUM-TS-DB2 PIC  X(026).
 

