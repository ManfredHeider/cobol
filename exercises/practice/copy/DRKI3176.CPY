      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3176.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3176.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:45:22   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Oct 23 2017 09:11:54 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3176                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3176                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.10.2017 / 15:18:40 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3176
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3176.
           05  PI-DRKI3176-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3176-CKVTARIFKEY   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3176-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3176-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3176-CKVTARIFART   PIC  X(0001).
             10 PI-DRKI3176-CKVTARIFBEZ   PIC  X(0008).
             10 PI-DRKI3176-CKVTARIFLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3176-CKVTARIFNAME.
                49 PI-DRKI3176-CKVTARIFNAME-LEN PIC S9(4) COMP.
                49 PI-DRKI3176-CKVTARIFNAME-TEXT PIC X(30).
             10 PI-DRKI3176-CKVTARIFTEXT.
                49 PI-DRKI3176-CKVTARIFTEXT-LEN PIC S9(4) COMP.
                49 PI-DRKI3176-CKVTARIFTEXT-TEXT PIC X(500).
             10 PI-DRKI3176-CKVTARIFVERSION   PIC  X(0007).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3176-NI1.
           05 PI-DRKI3176-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3176-NI7.
           05 PI-DRKI3176-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3176.
           05  PO-DRKI3176-RC                     PIC  X(003).
               88 PO-DRKI3176-RC-OK               VALUE 'OK '.
               88 PO-DRKI3176-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

