      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3453.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3453.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:42:26   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.2   Oct 27 2018 15:25:22   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.1   Sep 19 2018 11:47:32   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.0   Sep 12 2018 07:56:50 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3453                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:00:01 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3453
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3453.
           05  PI-DRKI3453-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3453-G42FVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3453.
           05  PO-DRKI3453-RC                     PIC  X(003).
               88 PO-DRKI3453-RC-OK               VALUE 'OK '.
               88 PO-DRKI3453-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3453-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3453-G42FAUFTKONF1   PIC S9(3)V COMP-3.
             10 PO-DRKI3453-G42FAUFTKONF2   PIC S9(3)V COMP-3.
             10 PO-DRKI3453-G42FAUSLQSTEUER   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3453-G42FFBETRVERBR   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FFSAID   PIC  X(0008).
             10 PO-DRKI3453-G42FGEMFBETRAG   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FGUELTIGAB   PIC  X(0008).
             10 PO-DRKI3453-G42FGUELTIGBIS   PIC  X(0008).
             10 PO-DRKI3453-G42FKISTEBEHALT   PIC  X(0008).
             10 PO-DRKI3453-G42FKISTSATZ1   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3453-G42FKISTSATZ2   PIC S9(2)V9(2) COMP-3.
             10 PO-DRKI3453-G42FKONFESSION1   PIC  X(0002).
             10 PO-DRKI3453-G42FKONFESSION2   PIC  X(0002).
             10 PO-DRKI3453-G42FKZSTAUSL   PIC  X(0001).
             10 PO-DRKI3453-G42FKZVERBAKTIE   PIC  X(0001).
             10 PO-DRKI3453-G42FKZVERBSONST   PIC  X(0001).
             10 PO-DRKI3453-G42FNVGUELTIGBIS   PIC  X(0008).
             10 PO-DRKI3453-G42FSTUECKZINS   PIC S9(10)V9(2) COMP-3.
             10 PO-DRKI3453-G42FSUMERTRAG   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3453-G42FSUMKEST   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FSUMKIST   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FSUMSOLI   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FVERBVORJ   PIC S9(5)V9(2) COMP-3.
             10 PO-DRKI3453-G42FVERLUSTAKTI   PIC S9(7)V9(2) COMP-3.
             10 PO-DRKI3453-G42FVERLUSTSON   PIC S9(7)V9(2) COMP-3.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 

