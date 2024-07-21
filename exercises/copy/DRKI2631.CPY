      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2631.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2631.CPv  $
      *
      *   Rev 2.3   Apr 21 2015 14:02:54   t13ex27
      *AKI01352: VB-Nummer Prüfung 'neu' gestalten
      *
      *   Rev 2.2   Sep 03 2012 10:16:08   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.1   Aug 15 2012 11:55:00   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.0   Jun 26 2012 11:54:59 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2631                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2631                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.03.2015 / 15:54:24 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2631
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2631.
           05  PI-DRKI2631-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2631-VBULFDNR   PIC S9(4) COMP.
             10 PI-DRKI2631-VBUVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI2631-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2631-VBUAUSWDAT   PIC  X(010).
             10 PI-DRKI2631-VBUSTATUS   PIC  X(0001).
END   *
 
 
      *
       01  PO-DRKI2631.
           05  PO-DRKI2631-RC                     PIC  X(003).
               88 PO-DRKI2631-RC-OK               VALUE 'OK '.
               88 PO-DRKI2631-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

