      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.10.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3172.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3172.CPv  $
      *
      *   Rev 2.1   Nov 22 2017 09:44:56   t13ae31
      *AKI01549: CKV Tarifdetails Copies
      *
      *   Rev 2.0   Oct 23 2017 09:11:50 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3172                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3172                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         23.10.2017 / 15:18:36 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3172
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3172.
           05  PI-DRKI3172-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3172-CKVTARIFKEY   PIC S9(4) COMP.
END   *
 
      *
       01  PO-DRKI3172.
           05  PO-DRKI3172-RC                     PIC  X(003).
               88 PO-DRKI3172-RC-OK               VALUE 'OK '.
               88 PO-DRKI3172-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3172-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3172-CKVTARIFART   PIC  X(0001).
             10 PO-DRKI3172-CKVTARIFBEZ   PIC  X(0008).
             10 PO-DRKI3172-CKVTARIFLFDNR   PIC S9(4) COMP.
             10 PO-DRKI3172-CKVTARIFNAME.
                49 PO-DRKI3172-CKVTARIFNAME-LEN PIC S9(4) COMP.
                49 PO-DRKI3172-CKVTARIFNAME-TEXT PIC X(30).
             10 PO-DRKI3172-CKVTARIFTEXT.
                49 PO-DRKI3172-CKVTARIFTEXT-LEN PIC S9(4) COMP.
                49 PO-DRKI3172-CKVTARIFTEXT-TEXT PIC X(500).
             10 PO-DRKI3172-CKVTARIFVERSION   PIC  X(0007).
             10 PO-DRKI3172-LETZTEAENDERUNG   PIC  X(0026).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

