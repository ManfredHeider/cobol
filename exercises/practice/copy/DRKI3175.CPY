      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3175.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3175.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:45:14   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Oct 23 2017 09:11:52 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3175                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3175                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.10.2017 / 15:18:38 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01549!04.10.17!1.0 ! Neu                       ! HHU       *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3175
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3175.
           05  PI-DRKI3175-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3175-CKVTARIFKEY   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3175-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3175-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3175-CKVTARIFART   PIC  X(0001).
             10 PI-DRKI3175-CKVTARIFBEZ   PIC  X(0008).
             10 PI-DRKI3175-CKVTARIFLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3175-CKVTARIFNAME.
                49 PI-DRKI3175-CKVTARIFNAME-LEN PIC S9(4) COMP.
                49 PI-DRKI3175-CKVTARIFNAME-TEXT PIC X(30).
             10 PI-DRKI3175-CKVTARIFTEXT.
                49 PI-DRKI3175-CKVTARIFTEXT-LEN PIC S9(4) COMP.
                49 PI-DRKI3175-CKVTARIFTEXT-TEXT PIC X(500).
             10 PI-DRKI3175-CKVTARIFVERSION   PIC  X(0007).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3175-NI1.
           05 PI-DRKI3175-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3175-NI7.
           05 PI-DRKI3175-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3175.
           05  PO-DRKI3175-RC                     PIC  X(003).
               88 PO-DRKI3175-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3175-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

