      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2776.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2776.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:49:00   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:33:52 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2776                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2776                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 16:33:52 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2776
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2776.
           05  PI-DRKI2776-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2776-BZKDBPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2776-BZKGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2776-BZKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2776-BZKVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2776-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2776-BZKABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2776-BZKANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2776-BZKANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2776-BZKBIC   PIC  X(0011).
             10 PI-DRKI2776-BZKGEBDATUM   PIC  X(010).
             10 PI-DRKI2776-BZKIBAN   PIC  X(0034).
             10 PI-DRKI2776-BZKMANDATREFNR.
                49 PI-DRKI2776-BZKMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2776-BZKMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2776-BZKNACHNAME   PIC  X(0030).
             10 PI-DRKI2776-BZKVORNAME   PIC  X(0030).
END   *


      *
       01  PO-DRKI2776.
           05  PO-DRKI2776-RC                     PIC  X(003).
               88 PO-DRKI2776-RC-OK               VALUE 'OK '.
               88 PO-DRKI2776-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
