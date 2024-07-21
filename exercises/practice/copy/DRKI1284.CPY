      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1284.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1284.CPv  $
      *
      *   Rev 2.1   May 03 2006 12:33:04   t13ex39
      *AKI00215: Umstellung auf EURO UMSATZ
      *
      *   Rev 2.0   05 Jul 2001 17:07:22   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1284                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1284                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.45.35
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1284
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1284.
           05  PI-DRKI1284-KEY.


             10 PI-DRKI1284-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1284-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI1284-UMSVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1284.
           05  PO-DRKI1284-RC                 PIC  X(003).
               88 PO-DRKI1284-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1284-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1284-DATEN.


             10 PO-DRKI1284-UMSBUCHUNGSDATUM   PIC X(10).
             10 PO-DRKI1284-UMSTEXT.
                15 PO-DRKI1284-UMSTEXT-LEN   PIC S9(4) COMP.
                15 PO-DRKI1284-UMSTEXT-TEXT   PIC X(20).

             10 PO-DRKI1284-UMSTEXTID   PIC S9(4) COMP.
             10 PO-DRKI1284-UMSUMSATZBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1284-NI-UMSUMSATZBETRAG   PIC S9(4) COMP.
             10 PO-DRKI1284-UMSUMSBETRWKZ   PIC X(01).
             10 PO-DRKI1284-UMSZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PO-DRKI1284-N-MSZHLNGSWSD   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
