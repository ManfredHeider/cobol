      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2731.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2731.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:06   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 13:58:42   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs f}r SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:45:35 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2731                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE F!R DIE FUNKTION DRKI2731                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 14:34:47 / T13EX19
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
      *    F!R DIE FUNKTION DRKI2731
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2731.
           05  PI-DRKI2731-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2731-BZDGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2731-BZDLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2731-BZDVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2731-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2731-BZDABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2731-BZDANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2731-BZDANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2731-BZDBIC   PIC  X(0011).
             10 PI-DRKI2731-BZDGEBDATUM   PIC  X(010).
             10 PI-DRKI2731-BZDIBAN   PIC  X(0034).
             10 PI-DRKI2731-BZDMANDATREFNR.
                49 PI-DRKI2731-BZDMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2731-BZDMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2731-BZDNACHNAME   PIC  X(0030).
             10 PI-DRKI2731-BZDVORNAME   PIC  X(0030).
END   *


      *
       01  PO-DRKI2731.
           05  PO-DRKI2731-RC                     PIC  X(003).
               88 PO-DRKI2731-RC-OK               VALUE 'OK '.
               88 PO-DRKI2731-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
