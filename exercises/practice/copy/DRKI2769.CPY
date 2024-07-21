      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2769.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2769.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 10:48:26   t13ex19
      *AKI01204: SEPA - DB-Online - OZPs für TCKI0458 / 0459
      *
      *   Rev 2.0   Jul 11 2013 16:29:23 T13EX19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2769                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2769                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.08.2013 / 09:37:46 / T13EX19
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
      *    FÜR DIE FUNKTION DRKI2769
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2769.
           05  PI-DRKI2769-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2769-BZBDBPGHPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2769-BZBDBPLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2769-BZBDBPVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2769-BZBLFDNR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2769.
           05  PO-DRKI2769-RC                     PIC  X(003).
               88 PO-DRKI2769-RC-OK               VALUE 'OK '.
               88 PO-DRKI2769-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2769-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2769-BZBABWBTGZAHLER   PIC  X(0001).
             10 PO-DRKI2769-BZBADRESSEID   PIC  X(0013).
             10 PO-DRKI2769-BZBANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2769-BZBANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2769-BZBBIC   PIC  X(0011).
             10 PO-DRKI2769-BZBGEBDATUM   PIC  X(010).
             10 PO-DRKI2769-BZBIBAN   PIC  X(0034).
             10 PO-DRKI2769-BZBKNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2769-BZBMANDATREFNR.
                49 PO-DRKI2769-BZBMANDATREFNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2769-BZBMNDTRFNR-TEXT PIC X(35).
             10 PO-DRKI2769-BZBNACHNAME   PIC  X(0030).
             10 PO-DRKI2769-BZBVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
