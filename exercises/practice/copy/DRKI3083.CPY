      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3083.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3083.CPv  $
      *
      *   Rev 2.4   May 24 2018 10:24:42   t13ex78
      *AKI01587: OZP's
      *
      *   Rev 2.3   Dec 06 2016 15:13:08   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:15:56   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:34:30   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Sep 1 2016 12:05:36 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3083                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3083                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.05.2018 / 10:04:44 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3083
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3083.
           05  PI-DRKI3083-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3083-VBSMITARBEITERNR   PIC S9(9) COMP.
END   *
 
      *
       01  PO-DRKI3083.
           05  PO-DRKI3083-RC                     PIC  X(003).
               88 PO-DRKI3083-RC-OK               VALUE 'OK '.
               88 PO-DRKI3083-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3083-NI.
               10 NI-DRKI3083-TAB.
                  15 DRKI3083-ELE
                     PIC S9(04) COMP OCCURS 33 TIMES.
               10 NI-DRKI3083 REDEFINES NI-DRKI3083-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI3083-NI-VBSAUSLGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSAUSLPRIV   PIC S9(4) COMP.
             15 PO-DRKI3083-N-VBSSTRTTSDTM   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSEDIDATUM   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSEDISTATUS   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSFAXGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSFAXPRIVAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSFONGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSFONPRIVAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSGEBGRUENDDAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSMANDANTNRALT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSMOBILGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSMOBILPRIVAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSORTGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSORTPRIV   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSPLZGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSPLZPRIV   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSPROVSATZ   PIC S9(4) COMP.
             15 PO-DRKI3083-N-VBSRCKSTFDTM   PIC S9(4) COMP.
             15 PO-DRKI3083-N-VBSSTRSSNRGSCH   PIC S9(4) COMP.
             15 PO-DRKI3083-N-VBSSTRSSNRPRV   PIC S9(4) COMP.
             15 PO-DRKI3083-N-VBSSTRKTRSTF   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSUMSTUFDATUM   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSVORGESPERSID   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSVORNAME   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSZUSTELLGESCH   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBSZUSTELLPRIV   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34CDAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34DREGISTNR   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34FDAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34FREGISTNR   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34IDAT   PIC S9(4) COMP.
             15 PO-DRKI3083-NI-VBS34IREGISTNR   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI3083-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3083-VBSANREDE   PIC S9(4) COMP.
             10 PO-DRKI3083-VBSAUSLGESCH   PIC S9(4) COMP.
             10 PO-DRKI3083-VBSAUSLPRIV   PIC S9(4) COMP.
             10 PO-DRKI3083-VBSAUSTRITTSDATUM   PIC  X(010).
             10 PO-DRKI3083-VBSDIREKTION   PIC S9(4) COMP.
             10 PO-DRKI3083-VBSEDIDATUM   PIC  X(010).
             10 PO-DRKI3083-VBSEDISTATUS   PIC  X(0001).
             10 PO-DRKI3083-VBSFAXGESCH   PIC  X(0026).
             10 PO-DRKI3083-VBSFAXPRIVAT   PIC  X(0026).
             10 PO-DRKI3083-VBSFONGESCH   PIC  X(0026).
             10 PO-DRKI3083-VBSFONPRIVAT   PIC  X(0026).
             10 PO-DRKI3083-VBSGEBGRUENDDAT   PIC  X(010).
             10 PO-DRKI3083-VBSMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI3083-VBSMANDANTNRALT   PIC S9(9) COMP.
             10 PO-DRKI3083-VBSMOBILGESCH   PIC  X(0026).
             10 PO-DRKI3083-VBSMOBILPRIVAT   PIC  X(0026).
             10 PO-DRKI3083-VBSNAME   PIC  X(0040).
             10 PO-DRKI3083-VBSORTGESCH   PIC  X(0040).
             10 PO-DRKI3083-VBSORTPRIV   PIC  X(0040).
             10 PO-DRKI3083-VBSPERSONENID   PIC  X(0013).
             10 PO-DRKI3083-VBSPLZGESCH   PIC  X(0009).
             10 PO-DRKI3083-VBSPLZPRIV   PIC  X(0009).
             10 PO-DRKI3083-VBSPROVSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI3083-VBSRUECKSTUFDATUM   PIC  X(010).
             10 PO-DRKI3083-VBSSTRASSENRGESCH   PIC  X(0040).
             10 PO-DRKI3083-VBSSTRASSENRPRIV   PIC  X(0040).
             10 PO-DRKI3083-VBSSTRUKTURSTUFE   PIC S9(9) COMP.
             10 PO-DRKI3083-VBSUMSTUFDATUM   PIC  X(010).
             10 PO-DRKI3083-VBSVBARTKZ   PIC  X(0001).
             10 PO-DRKI3083-VBSVORGESPERSID   PIC  X(0013).
             10 PO-DRKI3083-VBSVORNAME   PIC  X(0020).
             10 PO-DRKI3083-VBSZUSTELLGESCH   PIC S9(4) COMP.
             10 PO-DRKI3083-VBSZUSTELLPRIV   PIC S9(4) COMP.
             10 PO-DRKI3083-VBS34CDAT   PIC  X(010).
             10 PO-DRKI3083-VBS34DREGISTNR   PIC  X(0015).
             10 PO-DRKI3083-VBS34FDAT   PIC  X(010).
             10 PO-DRKI3083-VBS34FREGISTNR   PIC  X(0015).
             10 PO-DRKI3083-VBS34IDAT   PIC  X(010).
             10 PO-DRKI3083-VBS34IREGISTNR   PIC  X(0015).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

