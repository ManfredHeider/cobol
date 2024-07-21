      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.03.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3338.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3338.CPv  $
      *
      *   Rev 2.1   May 24 2018 08:07:44   t13ae31
      *AKI01575: G70 Umstellung KSDS auf DB2 OZP Umsatz
      *
      *   Rev 2.0   Mar 20 2018 13:09:34   t13ae31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
120318* AKI1575 !12.03.18!    ! NEU                       ! P. Hübner*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3338
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3338.
           05  PI-DRKI3338-PROGRAMM     PIC X(008).
           05  PI-DRKI3338-UFKT-CODE    PIC 9(002).
 
           05  PI-DRKI3338-DATEN.
ANF   *        %M_PI_KEY%
             10 PI-DRKI3338-G70UMANDANT      PIC  S9(009) COMP.
END   *
 
      *
       01  PO-DRKI3338.
           05  PO-DRKI3338-RC                     PIC  X(003).
               88 PO-DRKI3338-RC-OK               VALUE 'OK '.
               88 PO-DRKI3338-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI3338-RC-EOF              VALUE 'EOF'.
 
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3338-DATEN.
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3338-G70VVERTRAGSNR    PIC X(015).
             10 PO-DRKI3338-G70UVERTRAGSNR    PIC X(015).
             10 PO-DRKI3338-NI-G70UVERTRAGSNR PIC S9(4) COMP.
             10 PO-DRKI3338-G70ULFDNR         PIC X(001).
             10 PO-DRKI3338-G70UAENDKZ        PIC  X(0002).
             10 PO-DRKI3338-G70UBSPGUTHAB     PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3338-G70UFREIBETR      PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3338-G70UKUMANZUL      PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3338-G70UKUMWOBAUPR    PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3338-G70ULETZTAEND     PIC  X(0008).
             10 PO-DRKI3338-G70UMSATZTAB.
                49 PO-DRKI3338-G70UMSATZTAB-LEN  PIC S9(4) COMP.
                49 PO-DRKI3338-G70UMSATZTAB-TEXT PIC X(105).
             10 PO-DRKI3338-G70UPRODSCHL      PIC  X(0002).
             10 PO-DRKI3338-G70USTANDDARL     PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI3338-G70UUMSWKZ        PIC  X(0003).
END   *
      *
      ******* ENDE COPY-MEMBER ***************************************

