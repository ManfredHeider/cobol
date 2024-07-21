      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0690.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0690.CPv  $
      *
      *   Rev 2.2   Jun 24 2010 11:58:56   t13ex14
      *AKI00967: Badenia: OZPs und Copies
      *
      *   Rev 2.1   Apr 08 1999 16:23:28   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:24   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   25 Aug 1998 09:30:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0690                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0690                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
020610* AKI00967!02.06.10!    ! neues Feld UMSVWLUMSATZKZ ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0690
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0690.
           05  PI-DRKI0690-KEY.


             10 PI-DRKI0690-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0690-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI0690-UMSVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0690.
           05  PO-DRKI0690-RC                 PIC  X(003).
               88 PO-DRKI0690-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0690-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0690-DATEN.


             10 PO-DRKI0690-UMSBUCHUNGSDATUM   PIC X(10).
             10 PO-DRKI0690-UMSTEXT.
                15 PO-DRKI0690-UMSTEXT-LEN   PIC S9(4) COMP.
                15 PO-DRKI0690-UMSTEXT-TEXT   PIC X(20).

             10 PO-DRKI0690-UMSTEXTID   PIC S9(4) COMP.
             10 PO-DRKI0690-UMSUMSATZBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI0690-UMSUMSBETRWKZ   PIC X(01).
             10 PO-DRKI0690-UMSZAHLUNGSWEISEID   PIC S9(4) COMP.
020610       10 PO-DRKI0690-UMSVWLUMSATZKZ       PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
