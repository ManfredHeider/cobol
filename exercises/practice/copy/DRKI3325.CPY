      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3325.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3325.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:07:00   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Umsatz
      *
      *   Rev 2.0   Mar 13 2018 09:39:35 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3325                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3325                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.03.2018 / 10:41:36 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3325
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3325.
           05  PI-DRKI3325-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3325-G70ULFDNR   PIC  X(0001).
             10 PI-DRKI3325-G70UVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3325-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3325-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3325-G70UAENDKZ   PIC  X(0002).
             10 PI-DRKI3325-G70UBSPGUTHAB   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3325-G70UFREIBETR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3325-G70UKUMANZUL   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3325-G70UKUMWOBAUPR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3325-G70ULETZTAEND   PIC  X(0008).
             10 PI-DRKI3325-G70UMSATZTAB.
                49 PI-DRKI3325-G70UMSATZTAB-LEN PIC S9(4) COMP.
                49 PI-DRKI3325-G70UMSATZTAB-TEXT PIC X(105).
             10 PI-DRKI3325-G70UPRODSCHL   PIC  X(0002).
             10 PI-DRKI3325-G70USTANDDARL   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI3325-G70UUMSWKZ   PIC  X(0003).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3325-NI1.
           05 PI-DRKI3325-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3325-NI7.
           05 PI-DRKI3325-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3325.
           05  PO-DRKI3325-RC                     PIC  X(003).
               88 PO-DRKI3325-RC-OK               VALUE 'OK '.
               88 PO-DRKI3325-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

