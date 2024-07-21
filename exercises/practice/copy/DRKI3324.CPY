      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3324.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3324.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:06:54   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Umsatz
      *
      *   Rev 2.0   Mar 13 2018 09:39:33 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3324                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3324                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.03.2018 / 10:41:34 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI3324
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3324.
           05  PI-DRKI3324-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3324-G70ULFDNR   PIC  X(0001).
             10 PI-DRKI3324-G70UVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3324.
           05  PO-DRKI3324-RC                     PIC  X(003).
               88 PO-DRKI3324-RC-OK               VALUE 'OK '.
               88 PO-DRKI3324-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3324-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3324-G70UAENDKZ   PIC  X(0002).
             10 PO-DRKI3324-G70UBSPGUTHAB   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3324-G70UFREIBETR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3324-G70UKUMANZUL   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3324-G70UKUMWOBAUPR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3324-G70ULETZTAEND   PIC  X(0008).
             10 PO-DRKI3324-G70UMSATZTAB.
                49 PO-DRKI3324-G70UMSATZTAB-LEN PIC S9(4) COMP.
                49 PO-DRKI3324-G70UMSATZTAB-TEXT PIC X(105).
             10 PO-DRKI3324-G70UPRODSCHL   PIC  X(0002).
             10 PO-DRKI3324-G70USTANDDARL   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3324-G70UUMSWKZ   PIC  X(0003).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

