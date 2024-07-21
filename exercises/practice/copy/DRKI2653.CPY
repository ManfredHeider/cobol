      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2653.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2653.CPv  $
      *
      *   Rev 2.2   Jun 19 2017 09:39:26   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.1   May 23 2017 10:51:38   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.0   Apr 18 2017 10:27:02 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2653                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2653                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.06.2017 / 08:04:20 / T13AC17
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2653
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2653.
           05  PI-DRKI2653-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2653-LRVVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2653-LRVVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2653-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2653-NI-LRVBETBWRRKBUZV  PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTBWRRKBZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBETBWRRKEUZV  PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTBWRRKZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBETBWRRKHV    PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTBWRRKHVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBETBWRRKPZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTBWRRKPZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBETBWRRKTZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTBWRRKTZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSTWRTBZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSTWRTZV    PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSTWRTPZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSTWRTTZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRMNDWRTBZ   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRMNDWRTZ    PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRMNDWRTPZV  PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRMNDWRTTZV  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBTFRSUMBUZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSMBZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBTFRSUMEUZV   PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSMZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBTFRSUMPZV    PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSMPZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVBTFRSUMTZV    PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVBTFRSMTZVBKZ   PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVFMBTFRTTBUZV  PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2653-N-LRVFMBTFRTTBZVBK  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVFMBTFRTTEUZV  PIC S9(4) COMP.
             15 PI-DRKI2653-N-LRVFMBTFRTTZVBKZ  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVFMBTFRTTPZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2653-N-LRVFMBTFRTTPZVBK  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVFMBTFRTTTZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PI-DRKI2653-N-LRVFMBTFRTTTZVBK  PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWBUZV       PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWBUZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWEUZV       PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWEUZVBKZ    PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWHV         PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWHVBKZ      PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWPZV        PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWPZVBKZ     PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWTZV        PIC S9(4) COMP.
             15 PI-DRKI2653-NI-LRVRKWTZVBKZ     PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2653-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2653-LRVBETBWRRKBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBETBWRRKBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBETBWRRKEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBETBWRRKEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBETBWRRKHV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBETBWRRKHVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBETBWRRKPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBETBWRRKPZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBETBWRRKTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBETBWRRKTZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBTFRISTWERTBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRISTWERTEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRISTWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRISTWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRMINDWERTBUZ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRMINDWERTEUZ   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRMINDWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRMINDWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRSUMBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRSUMBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBTFRSUMEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRSUMEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBTFRSUMPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRSUMPZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVBTFRSUMTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVBTFRSUMTZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVFMBTFRTTBUZV   PIC  X(010).
             10 PI-DRKI2653-LRVFMBTFRTTBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVFMBTFRTTEUZV   PIC  X(010).
             10 PI-DRKI2653-LRVFMBTFRTTEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVFMBTFRTTPZV   PIC  X(010).
             10 PI-DRKI2653-LRVFMBTFRTTPZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVFMBTFRTTTZV   PIC  X(010).
             10 PI-DRKI2653-LRVFMBTFRTTTZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVRKWBUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVRKWBUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVRKWEUZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVRKWEUZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVRKWHV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVRKWHVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVRKWPZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVRKWPZVBKZ   PIC  X(0001).
             10 PI-DRKI2653-LRVRKWTZV   PIC S9(11)V9(2) COMP-3.
             10 PI-DRKI2653-LRVRKWTZVBKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2653-NI1.
           05 PI-DRKI2653-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 44.
       01  PI-DRKI2653-NI7.
           05 PI-DRKI2653-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 44.
 
      *
       01  PO-DRKI2653.
           05  PO-DRKI2653-RC                     PIC  X(003).
               88 PO-DRKI2653-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2653-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

