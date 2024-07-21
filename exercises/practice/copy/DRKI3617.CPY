      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3617.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3617.CPv  $
      *
      *   Rev 2.2   Jan 22 2020 10:46:40   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen 
      *
      *   Rev 2.1   Mar 18 2019 17:42:32   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.0   Jan 25 2019 13:06:55 T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3617                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3617                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.01.2020 / 10:25:25 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3617
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3617.
           05  PI-DRKI3617-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3617-G48KKOUKONTONR   PIC  X(0002).
             10 PI-DRKI3617-G48KKOVNR   PIC  X(0015).
             10 PI-DRKI3617-G48KKOWAEHRUNG   PIC  X(0003).
END   *

      *
       01  PO-DRKI3617.
           05  PO-DRKI3617-RC                     PIC  X(003).
               88 PO-DRKI3617-RC-OK               VALUE 'OK '.
               88 PO-DRKI3617-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI3617-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3617-G48KKOBIC   PIC  X(0011).
             10 PO-DRKI3617-G48KKOBILANZSCHL   PIC  X(0004).
             10 PO-DRKI3617-G48KKODATENDEBEG   PIC  X(0008).
             10 PO-DRKI3617-G48KKODATOEFF   PIC  X(0008).
             10 PO-DRKI3617-G48KKODATSALDO   PIC  X(0008).
             10 PO-DRKI3617-G48KKODELKZ   PIC  X(0001).
             10 PO-DRKI3617-G48KKOEXTLIMIT   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3617-G48KKOIBAN   PIC  X(0034).
             10 PO-DRKI3617-G48KKOIBANKK   PIC  X(0034).
             10 PO-DRKI3617-G48KKOINHABER   PIC  X(0028).
             10 PO-DRKI3617-G48KKOKENNZSCHL   PIC  X(0002).
             10 PO-DRKI3617-G48KKOKFWDATFEST   PIC  X(0008).
             10 PO-DRKI3617-G48KKOKFWERSTR   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKFWFOLGER   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKFWKREDITB   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKFWSOLLZ   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3617-G48KKOKFWTILGUNG   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3617-G48KKOKONDSCHL   PIC  X(0002).
             10 PO-DRKI3617-G48KKOKOSTSCHL   PIC  X(0001).
             10 PO-DRKI3617-G48KKOKUMHABENAM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKUMHABENVM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKUMSOLLAM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOKUMSOLLVM   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOMANDREF   PIC  X(0035).
             10 PO-DRKI3617-G48KKOPIN   PIC  X(0009).
             10 PO-DRKI3617-G48KKOSALDO   PIC S9(12)V9(2) COMP-3.
             10 PO-DRKI3617-G48KKOSCHLDAT   PIC  X(0008).
             10 PO-DRKI3617-G48KKOSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3617-G48KKOUEBERZINS   PIC S9(2)V9(4) COMP-3.
             10 PO-DRKI3617-G48KKOULTOSPARKZ   PIC  X(0001).
             10 PO-DRKI3617-G48KKOZRAM   PIC  X(0005).
             10 PO-DRKI3617-G48KKOZRVM   PIC  X(0005).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

