      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3069.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3069.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:09:16   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung OZP
      *
      *   Rev 2.0   Apr 21 2016 10:56:53 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3069                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3069                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.04.2016 / 10:56:53 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3069
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3069.
           05  PI-DRKI3069-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3069-RVSVERTRAGSID   PIC  X(0013).
             10 PI-DRKI3069-RVSVETLFDNR   PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3069-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3069-N-RVSBDNGNG-KNR    PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSBDNGNG-KTY    PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSGSLLSCH-KNR   PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSGSLLSCH-KTY   PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSNFQLL-KNR     PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSNFQLL-KTY     PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSMAILING-KNR  PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSMAILING-KTY  PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSVERSNUMMER   PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSVSUMRGEPCK   PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSVSUMRUECK    PIC S9(4) COMP.
             15 PI-DRKI3069-N-RVSVMSTLLNGDT    PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSZUSINFO-KNR  PIC S9(4) COMP.
             15 PI-DRKI3069-NI-RVSZUSINFO-KTY  PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3069-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3069-RVSBEDINGUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3069-RVSBEDINGUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI3069-RVSDIFDECKKZ   PIC  X(0001).
             10 PI-DRKI3069-RVSEINZVTGHOSTID   PIC  X(0015).
             10 PI-DRKI3069-RVSGESELLSCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3069-RVSGESELLSCH-KTY   PIC S9(4) COMP.
             10 PI-DRKI3069-RVSINFOQUELLE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3069-RVSINFOQUELLE-KTY   PIC S9(4) COMP.
             10 PI-DRKI3069-RVSKUENDIGUNGKZ   PIC  X(0001).
             10 PI-DRKI3069-RVSMAILING-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3069-RVSMAILING-KTY   PIC S9(4) COMP.
             10 PI-DRKI3069-RVSVERSNUMMER   PIC  X(0040).
             10 PI-DRKI3069-RVSVSUMRGEPCK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI3069-RVSVSUMRUECK   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI3069-RVSVUMSTELLUNGDAT   PIC  X(010).
             10 PI-DRKI3069-RVSZUSINFO-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI3069-RVSZUSINFO-KTY   PIC S9(4) COMP.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3069-NI1.
           05 PI-DRKI3069-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 14.
       01  PI-DRKI3069-NI7.
           05 PI-DRKI3069-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 14.
 
      *
       01  PO-DRKI3069.
           05  PO-DRKI3069-RC                     PIC  X(003).
               88 PO-DRKI3069-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3069-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

