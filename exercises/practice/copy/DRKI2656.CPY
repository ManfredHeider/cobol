      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.04.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2656.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2656.CPv  $
      *
      *   Rev 2.1   May 23 2017 10:52:12   t13ac17
      *AKI01510: Standard OZP
      *
      *   Rev 2.0   Apr 18 2017 10:27:10 T13AC17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2656                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2656                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2017 / 08:32:10 / T13AC17
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01510!18.04.17!2.1 ! Neuerstellung             ! GWA      *
      *         !        !    ! man. Anpassung wg. Wort-  !          *
      *         !        !    ! länge                     !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2656
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2656.
           05  PI-DRKI2656-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2656-LRVVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2656-LRVVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2656.
           05  PO-DRKI2656-RC                     PIC  X(003).
               88 PO-DRKI2656-RC-OK               VALUE 'OK '.
               88 PO-DRKI2656-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2656-NI.
               10 NI-DRKI2656-TAB.
                  15 DRKI2656-ELE
                     PIC S9(04) COMP OCCURS 45 TIMES.
               10 NI-DRKI2656 REDEFINES NI-DRKI2656-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2656-NI-LETZTEAENDERUNG   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBETBWRRKBUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTBWRRKBZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBETBWRRKEUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTBWRRKZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBETBWRRKHV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTBWRRKHVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBETBWRRKPZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTBWRRKPZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBETBWRRKTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTBWRRKTZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSTWRTBZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSTWRTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSTWRTPZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSTWRTTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRMNDWRTBZ   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRMNDWRTZ   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRMNDWRTPZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRMNDWRTTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBTFRSUMBUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSMBZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBTFRSUMEUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSMZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBTFRSUMPZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSMPZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVBTFRSUMTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVBTFRSMTZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVFMBTFRTTBUZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PO-DRKI2656-N-LRVFMBTFRTTBZVBK    PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVFMBTFRTTEUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-N-LRVFMBTFRTTZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVFMBTFRTTPZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PO-DRKI2656-N-LRVFMBTFRTTPZVBK    PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVFMBTFRTTTZV   PIC S9(4) COMP.
180417*  Angepasst wg. Wortlänge BK statt BKZ
             15 PO-DRKI2656-N-LRVFMBTFRTTTZVBK    PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWBUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWBUZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWEUZV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWEUZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWHV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWHVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWPZV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWPZVBKZ   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWTZV   PIC S9(4) COMP.
             15 PO-DRKI2656-NI-LRVRKWTZVBKZ   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI2656-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2656-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI2656-LRVBETBWRRKBUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBETBWRRKBUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBETBWRRKEUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBETBWRRKEUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBETBWRRKHV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBETBWRRKHVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBETBWRRKPZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBETBWRRKPZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBETBWRRKTZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBETBWRRKTZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBTFRISTWERTBUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRISTWERTEUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRISTWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRISTWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRMINDWERTBUZ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRMINDWERTEUZ   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRMINDWERTPZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRMINDWERTTZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRSUMBUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRSUMBUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBTFRSUMEUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRSUMEUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBTFRSUMPZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRSUMPZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVBTFRSUMTZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVBTFRSUMTZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVFMBTFRTTBUZV   PIC  X(010).
             10 PO-DRKI2656-LRVFMBTFRTTBUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVFMBTFRTTEUZV   PIC  X(010).
             10 PO-DRKI2656-LRVFMBTFRTTEUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVFMBTFRTTPZV   PIC  X(010).
             10 PO-DRKI2656-LRVFMBTFRTTPZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVFMBTFRTTTZV   PIC  X(010).
             10 PO-DRKI2656-LRVFMBTFRTTTZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVRKWBUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVRKWBUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVRKWEUZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVRKWEUZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVRKWHV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVRKWHVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVRKWPZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVRKWPZVBKZ   PIC  X(0001).
             10 PO-DRKI2656-LRVRKWTZV   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2656-LRVRKWTZVBKZ   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

