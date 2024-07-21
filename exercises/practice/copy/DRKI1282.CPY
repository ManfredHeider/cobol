      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1282.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1282.CPv  $
      *
      *   Rev 2.2   Apr 04 2017 14:23:02   t13ac20
      *AKI01503: OZPs bereinigen
      *
      *   Rev 2.1   May 03 2006 13:11:42   t13ex39
      *AKI00215: Umstellung aud EURO  LEBEN GESAMT
      *
      *   Rev 2.0   05 Jul 2001 17:07:20   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1282                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1282                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         30.03.2017 / 15:43:16 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1282
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1282.
           05  PI-DRKI1282-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI1282-LTLLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI1282-LTLVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1282-LTLVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI1282.
           05  PO-DRKI1282-RC                     PIC  X(003).
               88 PO-DRKI1282-RC-OK               VALUE 'OK '.
               88 PO-DRKI1282-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1282-NI.
               10 NI-DRKI1282-TAB.
                  15 DRKI1282-ELE
                     PIC S9(04) COMP OCCURS 2 TIMES.
               10 NI-DRKI1282 REDEFINES NI-DRKI1282-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI1282-N-LTLSZHLBTRG   PIC S9(4) COMP.
             15 PO-DRKI1282-N-LTLVRRNTKPBTR   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI1282-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI1282-LTLAUSZAHLBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1282-LTLAUSZAHLZEITPKT   PIC  X(010).
             10 PO-DRKI1282-LTLVERRENTKAPBETR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1282-LTLVERRENTKAPBWKZ   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

