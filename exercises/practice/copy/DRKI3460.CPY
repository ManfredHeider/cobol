      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3460.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3460.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:51:00   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.2   Oct 27 2018 15:28:46   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.1   Sep 21 2018 10:14:10   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.0   Sep 12 2018 08:06:02 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3460                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3460                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:20:22 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3460
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3460.
           05  PI-DRKI3460-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3460-G42PVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3460.
           05  PO-DRKI3460-RC                     PIC  X(003).
               88 PO-DRKI3460-RC-OK               VALUE 'OK '.
               88 PO-DRKI3460-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3460-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3460-G42PAKTMONAT   PIC  X(0005).
             10 PO-DRKI3460-G42PBUCHDAT   PIC  X(0008).
             10 PO-DRKI3460-G42PDATEROFFNUNG   PIC  X(0008).
             10 PO-DRKI3460-G42PDATSCHLIESS   PIC  X(0008).
             10 PO-DRKI3460-G42PEXTLIMIT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3460-G42PKONTOART   PIC  X(0002).
             10 PO-DRKI3460-G42PKUMHABENVM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PKUMSOLLVM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3460-G42PSUMHABENAM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PSUMSOLLAM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3460-G42PUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3460-G42PVORMONAT   PIC  X(0005).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 

