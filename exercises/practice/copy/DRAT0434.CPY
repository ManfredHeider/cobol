      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0434.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0434.CPv  $
      *
      *   Rev 2.2   Sep 17 2008 09:27:42   t13ex51
      *AKI00731: Anpassen für Lesen TBAT0039 mit UPKI
      *
      *   Rev 2.1   Oct 03 2007 12:18:40   t13ex51
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL 
      *
      *   Rev 2.0   Oct 01 2007 15:17:48   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0434                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0434                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!20.09.07!1.0 ! NEU                       ! HRO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0434
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0434.
           05  PI-DRAT0434-KEY.
             10 PI-DRAT0434-KTY               PIC S9(4) COMP.
             10 PI-DRAT0434-KNR               PIC S9(15)V USAGE COMP-3.
      *
       01  PO-DRAT0434.
           05  PO-DRAT0434-RC                 PIC  X(003).
               88 PO-DRAT0434-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0434-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0434-DATEN.
             10 PO-DRAT0434-BEZEICHNUNG          PIC X(100).
             10 PO-DRAT0434-BEZEICHNUNG-KURZ     PIC X(20).
      *
      ******* ENDE COPY-MEMBER **************************************
      *

