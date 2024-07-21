      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2630.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2630.CPv  $
      *
      *   Rev 2.3   Apr 21 2015 14:02:50   t13ex27
      *AKI01352: VB-Nummer Prüfung 'neu' gestalten
      *
      *   Rev 2.2   Sep 03 2012 10:16:00   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.1   Aug 15 2012 11:54:52   t13ex14
      *AKI01155: VerwFond berein. 1: OZPs f. VBUNGUELTIGAV, VIVB9996
      *
      *   Rev 2.0   Jun 26 2012 11:54:55 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2630                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2630                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.03.2015 / 12:06:23 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2630
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI2630.
           05  PI-DRKI2630-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2630-VBUVERTRAGSNR   PIC  X(0015).
END   *
 
 
           05  PI-DRKI2630-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2630-VBUABLEHNKZ   PIC  X(0001).
             10 PI-DRKI2630-VBUAUSTRITTSDAT   PIC  X(010).
             10 PI-DRKI2630-VBUMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2630-VBUPARTGESID   PIC S9(4) COMP.
             10 PI-DRKI2630-VBUSTATUS   PIC  X(0001).
             10 PI-DRKI2630-VBUSTRUKTURSTUFE   PIC S9(9) COMP.
             10 PI-DRKI2630-N-VBSTRKTRSTF      PIC S9(4) COMP.
             10 PI-DRKI2630-VBUVBNRGELIEF   PIC S9(9) COMP.
             10 PI-DRKI2630-NI-VBUVBNRGELIEF   PIC S9(4) COMP.
             10 PI-DRKI2630-VBUVRBVBNR   PIC S9(9) COMP.
             10 PI-DRKI2630-NI-VBUVRBVBNR      PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI2630.
           05  PO-DRKI2630-RC                     PIC  X(003).
               88 PO-DRKI2630-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2630-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI2630-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2630-VBULFDNR   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER **************************************

