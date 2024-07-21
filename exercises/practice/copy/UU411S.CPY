      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   UU411S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/UU411S.CPv  $
      *
      *   Rev 2.2   28 Mar 2007 15:57:26   t13ae23
      *AAN00655: Batchabläufe Täglich - Erweiterung des PS auf 3 St
      *ellen 
      *
      *   Rev 2.1   Oct 13 1999 16:39:50   T13AE34
      *FAN00227-Korr. Proderg
      *
      *   Rev 2.0   Feb 27 1999 15:15:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   17 Dec 1998 08:42:30   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.2   03 Sep 1998 13:10:42   T13EX36
      *AAN00164 - Jahr2000 Umstellung
      *
      *   Rev 1.1   Jun 03 1996 13:47:32   T13AE19
      *Anpassung an UM15-NEU
      *
      *   REV 1.0   NOV 04 1995 08:02:34   P13SP13
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *******     PRODUKTIONSERGEBNISSATZ FUER UM-15       *************
      *                                                              *
      ****************************************************************
      *  VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        ! 00 ! NEU                       ! ??       *
      *         !02.04.91! 00 ! EINFUEHRUNG STUFE         ! MUELLER  *
      * AFPC0008!......94! .. ! EINFUEHRUNG BETREUERNUMMER! MUELLER  *
      * FAN00227!13.10.99!2.1 ! KORR. PROD.ERG            ! HEIDER   *
      * AAN00655!28.03.07!2.2 ! DREISTELLIGE PRODUKTSCHL. ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  UU411S.
           10  NSA                     PIC 99.
           10  NKEY.
               20  NMANR               PIC 9(7)        COMP-3.
               20  NSTUFE              PIC 9(3).
               20  NBMANR              PIC 9(7)        COMP-3.
*Y2K***        20  NSTATDAT            PIC 9(6)        COMP-3.
               20  NSTATDAT            PIC 9(7)        COMP-3.
280307*        20  NSTATSCHL           PIC 9(5)        COMP-3.
280307         20  NSTATSCHL.
280307             30 NSTATSCHL-ERG-KZ         PIC 9(1).
280307             30 NSTATSCHL-PGEB-GES       PIC 9(003)  COMP-3.
280307             30 NSTATSCHL-PROD-PROD-SCHL PIC 9(003)  COMP-3.
      *
      *    PRODUKTIONS-ERGEBNIS-SATZ FUER UM-15.
           10  NSATZA.
           20  NEIGEN.
               30  NEAEINH             PIC S9(8)V9     COMP-3.
               30  NESTCK              PIC S9(7)       COMP-3.
               30  NESUM               PIC S9(11)      COMP-3.
131099*        30  NEBEITR             PIC S9(9)V99    COMP-3.
131099         30  NEBEITR             PIC S9(11)V99   COMP-3.
           20  NGRUPPE.
               30  NGRAEINH            PIC S9(8)V9     COMP-3.
               30  NGRSTCK             PIC S9(7)       COMP-3.
               30  NGRSUM              PIC S9(11)      COMP-3.
131099*        30  NGRBEITR            PIC S9(9)V99    COMP-3.
131099         30  NGRBEITR            PIC S9(11)V99   COMP-3.
      *    20  NLEER                   PIC X(6).
131099*    20  NLEER                   PIC X(2).
      *
CCC009*    SKIP2
      ******************************************************************
      *    *** LAENGE 64 BYTES ***
      ******************************************************************
      *

