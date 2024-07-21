      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3351.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3351.CPv  $
      *
      *   Rev 2.1   Jan 12 2021 15:38:48   u130684
      *AKI01775: Neues Ermittlungsprogramm Fondsdepot Bank
      *
      *   Rev 2.0   Apr 17 2020 09:49:44 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3351                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3351                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.12.2020 / 09:57:02 / U130684
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
      *    FÜR DIE FUNKTION DRKI3351
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3351.
           05  PI-DRKI3351-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3351-G40VERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3351.
           05  PO-DRKI3351-RC                     PIC  X(003).
               88 PO-DRKI3351-RC-OK               VALUE 'OK '.
               88 PO-DRKI3351-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3351-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3351-G40ANACHLASSKZ   PIC  X(0001).
             10 PO-DRKI3351-G40BANK   PIC  X(0020).
             10 PO-DRKI3351-G40BIC   PIC  X(0011).
             10 PO-DRKI3351-G40IBAN   PIC  X(0034).
             10 PO-DRKI3351-G40INTVERTNR   PIC S9(9) COMP.
             10 PO-DRKI3351-G40KONTOART   PIC  X(0002).
             10 PO-DRKI3351-G40KUNDENDATEN.
                49 PO-DRKI3351-G40KUNDENDATEN-LEN PIC S9(4) COMP.
                49 PO-DRKI3351-G40KNDNDTN-TEXT PIC X(322).
             10 PO-DRKI3351-G40MANDANT   PIC S9(9) COMP.
             10 PO-DRKI3351-G40RISIKOKLASSE   PIC  X(0001).
             10 PO-DRKI3351-G40VBNR   PIC S9(9) COMP.
             10 PO-DRKI3351-G40VERTABLAFFKZ   PIC  X(0001).
             10 PO-DRKI3351-G40VERTAENDDAT   PIC  X(0008).
             10 PO-DRKI3351-G40VERTBEGINNKZ   PIC  X(0001).
             10 PO-DRKI3351-G40VRTRGABLAUF   PIC  X(0008).
             10 PO-DRKI3351-G40VRTRGBEGHIST   PIC  X(0008).
             10 PO-DRKI3351-G40VRTRGBEGINN   PIC  X(0008).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

