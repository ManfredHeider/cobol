      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2729.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2729.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:46:56   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 13:58:26   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs f}r SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:45:26 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2729                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE F!R DIE FUNKTION DRKI2729                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 14:34:44 / T13EX19
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME     *
      * -------------------------------------------------------------*
      *         |        |    |                           |          *
      *         |        |    |                           |          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTH¢LT DIE EIN- UND AUSGABEPARAMETER
      *    F!R DIE FUNKTION DRKI2729
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2729.
           05  PI-DRKI2729-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2729-BZDGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2729-BZDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2729-BZDVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2729.
           05  PO-DRKI2729-RC                     PIC  X(003).
               88 PO-DRKI2729-RC-OK               VALUE 'OK '.
               88 PO-DRKI2729-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2729-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2729-BZDABWBTGZAHLER   PIC  X(0001).
             10 PO-DRKI2729-BZDADRESSEID   PIC  X(0013).
             10 PO-DRKI2729-BZDANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2729-BZDANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2729-BZDBIC   PIC  X(0011).
             10 PO-DRKI2729-BZDGEBDATUM   PIC  X(010).
             10 PO-DRKI2729-BZDIBAN   PIC  X(0034).
             10 PO-DRKI2729-BZDKNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2729-BZDMANDATREFNR.
                49 PO-DRKI2729-BZDMANDATREFNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2729-BZDMNDTRFNR-TEXT PIC X(35).
             10 PO-DRKI2729-BZDNACHNAME   PIC  X(0030).
             10 PO-DRKI2729-BZDVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
