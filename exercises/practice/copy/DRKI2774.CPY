      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2774.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2774.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:48:50   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:33:48 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2774                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2774                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.07.2013 / 16:33:48 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2774
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2774.
           05  PI-DRKI2774-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2774-BZKDBPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2774-BZKGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2774-BZKLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2774-BZKVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2774.
           05  PO-DRKI2774-RC                     PIC  X(003).
               88 PO-DRKI2774-RC-OK               VALUE 'OK '.
               88 PO-DRKI2774-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2774-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2774-BZKABWBTGZAHLER   PIC  X(0001).
             10 PO-DRKI2774-BZKADRESSEID   PIC  X(0013).
             10 PO-DRKI2774-BZKANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2774-BZKANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2774-BZKBIC   PIC  X(0011).
             10 PO-DRKI2774-BZKGEBDATUM   PIC  X(010).
             10 PO-DRKI2774-BZKIBAN   PIC  X(0034).
             10 PO-DRKI2774-BZKKNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2774-BZKMANDATREFNR.
                49 PO-DRKI2774-BZKMANDATREFNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2774-BZKMNDTRFNR-TEXT PIC X(35).
             10 PO-DRKI2774-BZKNACHNAME   PIC  X(0030).
             10 PO-DRKI2774-BZKVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
