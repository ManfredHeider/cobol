      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2654.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2654.CPv  $
      *
      *   Rev 2.2   Jun 19 2017 09:39:38   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.1   May 23 2017 10:51:48   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.0   Apr 18 2017 10:27:05 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2654                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2654                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.06.2017 / 08:04:23 / T13AC17
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01510!18.04.17!2.1 ! Neuerstellung             ! GWA      *
      *         !        !    ! man. Anpassung wg. Wort-  !          *
      *         !        !    ! länge                     !          *
      * AKI01510!19.06.17!2.2 ! Anpassung LETZTEAENDERUNG ! GWA      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2654
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2654.
           05  PI-DRKI2654-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2654-LRVVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2654-LRVVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2654-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2654-NI-LRVBETBWRRKBUZV  PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTBWRRKBZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBETBWRRKEUZV  PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTBWRRKZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBETBWRRKHV    PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTBWRRKHVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBETBWRRKPZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTBWRRKPZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBETBWRRKTZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTBWRRKTZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSTWRTBZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSTWRTZV    PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSTWRTPZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSTWRTTZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRMNDWRTBZ   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRMNDWRTZ    PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRMNDWRTPZV  PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRMNDWRTTZV  PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBTFRSUMBUZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSMBZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBTFRSUMEUZV   PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSMZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBTFRSUMPZV    PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSMPZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVBTFRSUMTZV    PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVBTFRSMTZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVFMBTFRTTBUZV  PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2654-N-LRVFMBTFRTTBZVBK  PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVFMBTFRTTEUZV  PIC S9(4) COMP.
             15 PI-DRKI2654-N-LRVFMBTFRTTZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVFMBTFRTTPZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2654-N-LRVFMBTFRTTPZVBK PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVFMBTFRTTTZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2654-N-LRVFMBTFRTTTZVBK PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWBUZV       PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWBUZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWEUZV       PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWEUZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWHV         PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWHVBKZ      PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWPZV        PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWPZVBKZ     PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWTZV        PIC S9(4) COMP.
             15 PI-DRKI2654-NI-LRVRKWTZVBKZ     PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2654-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2654-LRVBETBWRRKBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBETBWRRKBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBETBWRRKEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBETBWRRKEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBETBWRRKHV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBETBWRRKHVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBETBWRRKPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBETBWRRKPZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBETBWRRKTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBETBWRRKTZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBTFRISTWERTBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRISTWERTEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRISTWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRISTWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRMINDWERTBUZ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRMINDWERTEUZ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRMINDWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRMINDWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRSUMBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRSUMBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBTFRSUMEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRSUMEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBTFRSUMPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRSUMPZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVBTFRSUMTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVBTFRSUMTZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVFMBTFRTTBUZV   PIC  X(010).
             10 PI-DRKI2654-LRVFMBTFRTTBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVFMBTFRTTEUZV   PIC  X(010).
             10 PI-DRKI2654-LRVFMBTFRTTEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVFMBTFRTTPZV   PIC  X(010).
             10 PI-DRKI2654-LRVFMBTFRTTPZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVFMBTFRTTTZV   PIC  X(010).
             10 PI-DRKI2654-LRVFMBTFRTTTZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVRKWBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVRKWBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVRKWEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVRKWEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVRKWHV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVRKWHVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVRKWPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVRKWPZVBKZ   PIC  X(0001).
             10 PI-DRKI2654-LRVRKWTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2654-LRVRKWTZVBKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2654-NI1.
           05 PI-DRKI2654-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 44.
       01  PI-DRKI2654-NI7.
           05 PI-DRKI2654-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 44.
      *
       01  PO-DRKI2654.
           05  PO-DRKI2654-RC                     PIC  X(003).
               88 PO-DRKI2654-RC-OK               VALUE 'OK '.
               88 PO-DRKI2654-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

