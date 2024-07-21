      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2735.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2735.CPv  $
      *
      *   Rev 2.2   Aug 21 2013 10:47:28   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.1   Feb 27 2013 13:59:20   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:55:29 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2735                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2735                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 15:34:37 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2735
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2735.
           05  PI-DRKI2735-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2735-BZSGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2735-BZSGHZFONDSID   PIC S9(4) COMP.
             10 PI-DRKI2735-BZSGHZLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2735-BZSLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2735-BZSVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2735.
           05  PO-DRKI2735-RC                     PIC  X(003).
               88 PO-DRKI2735-RC-OK               VALUE 'OK '.
               88 PO-DRKI2735-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2735-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2735-BZSABWBTGZAHLER   PIC  X(0001).
             10 PO-DRKI2735-BZSADRESSEID   PIC  X(0013).
             10 PO-DRKI2735-BZSANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2735-BZSANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2735-BZSBIC   PIC  X(0011).
             10 PO-DRKI2735-BZSGEBDATUM   PIC  X(010).
             10 PO-DRKI2735-BZSIBAN   PIC  X(0034).
             10 PO-DRKI2735-BZSKNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2735-BZSMANDATREFNR.
                49 PO-DRKI2735-BZSMANDATREFNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2735-BZSMNDTRFNR-TEXT PIC X(35).
             10 PO-DRKI2735-BZSNACHNAME   PIC  X(0030).
             10 PO-DRKI2735-BZSVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
